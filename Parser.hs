{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser where

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.RWS (RWST(..), runRWST)
import Control.Monad.RWS.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Set as S
import Data.Bits
import Data.Bits.Lens
import Data.Bits.Floating
import Data.Word
import Data.Int
import Data.Monoid
import Linear

import Util
import Overture
import Prelude ()


data Endianness = LE | BE deriving (Eq, Ord, Read, Show, Enum, Bounded)
instance Display Endianness where
    display LE = "little-endian"
    display BE = "big-endian"

newtype Offset = Offset { fromOffset :: Int } deriving (Eq, Ord, Show, Read, Enum, Real, Integral, Num)
instance Display Offset where
    display = showHex

data Label = Label
    { lblPos :: Offset 
    , lblLen :: Offset
    , lblName :: Maybe String
    } deriving (Eq, Ord)

data Section = Section
    { sectionLabel :: Label
    , sectionValue :: Either String SectionList
    } deriving (Eq, Ord)

newtype SectionList = SL { fromSL :: [Section] } deriving (Eq, Ord)

instance Monoid SectionList where
    mempty = SL []
    mappend sl1 (SL []) = sl1
    mappend (SL []) sl2 = sl2
    mappend sl1@(SL (s1@(Section (Label p1 _ _) _):ss1)) 
            sl2@(SL (s2@(Section (Label p2 _ _) _):ss2))
        | p1 < p2   = SL $ s1 : fromSL (SL ss1 <> sl2)
        | otherwise = SL $ s2 : fromSL (sl1 <> SL ss2)

data Struct = Struct
    { structPos :: Offset
    , structLen :: Offset
    , structName :: String
    , structFields :: SectionList
    } deriving (Eq, Ord)

type ParseError = (Offset, String)

data FPState = FPState
    { _endianness :: Endianness
    , _offset :: Offset
    }

data ParserOut a = PO
    { _structs :: S.Set Struct
    , _parserOutData :: a
    }

type FPOut = ParserOut SectionList

type SPOut = ParserOut (Last Offset)

data ParserIn m a = ParserIn
    { _inputBytes :: BS.ByteString
    , _debugWrite :: Offset -> String -> m ()
    , _parserInData :: a
    }

instance (Monoid a) => Monoid (ParserOut a) where
    mempty = PO mempty mempty
    mappend (PO x1 y1) (PO x2 y2) = PO (x1 <> x2) (y1 <> y2)

makeLenses ''FPState
makeLenses ''ParserOut
makeLenses ''ParserIn

type Parser i o s m = ExceptT ParseError (RWST (ParserIn m i) (ParserOut o) s m)
type StructParser m = ExceptT ParseError (RWST (ParserIn m Offset) SPOut Endianness m)
type FieldParser m = ExceptT ParseError (RWST (ParserIn m ()) FPOut FPState m)

runParser :: (Monad m) => StructParser m a -> (String -> m ()) -> BS.ByteString -> m (Maybe a)
runParser p f bs = do
    (r, sts) <- runStructParserT p f' bs
    mapM_ f $ formatStructs bs sts
    case r of
        Left (o, err) -> f' o err >> return Nothing
        Right r' -> return (Just r')
  where f' o e = f $ showHex o ++ " : " ++ e

runStructParser :: StructParser Identity a -> BS.ByteString -> (Either ParseError a, S.Set Struct)
runStructParser p bs = runIdentity $ runStructParserT p (\_ _ -> return ()) bs

runStructParserT :: (Monad m) => StructParser m a -> (Offset -> String -> m ()) -> BS.ByteString 
                 -> m (Either ParseError a, S.Set Struct)
runStructParserT p dw bs = do
    (r, _, po) <- runRWST (runExceptT p) (ParserIn bs dw 0) LE
    return (r, po^.structs)



uint8 :: (Monad m) => FieldParser m Word8
uint8 = displayValue $ prim 1 Bin.getWord8 Bin.getWord8

uint16 :: (Monad m) => FieldParser m Word16
uint16 = displayValue $ prim 2 Bin.getWord16le Bin.getWord16be

uint32 :: (Monad m) => FieldParser m Word32
uint32 = displayValue $ prim 4 Bin.getWord32le Bin.getWord32be

uint64 :: (Monad m) => FieldParser m Word64
uint64 = displayValue $ prim 8 Bin.getWord64le Bin.getWord64be

int8 :: (Monad m) => FieldParser m Int8
int8 = displayValue $ liftM fromIntegral uint8

int16 :: (Monad m) => FieldParser m Int16
int16 = displayValue $ liftM fromIntegral uint16

int32 :: (Monad m) => FieldParser m Int32
int32 = displayValue $ liftM fromIntegral uint32

int64 :: (Monad m) => FieldParser m Int64
int64 = displayValue $ liftM fromIntegral uint64

int :: (Monad m) => FieldParser m Int
int = liftM fromIntegral int32

float32 :: (Monad m) => FieldParser m Float
float32 = displayValue $ liftM coerceToFloat uint32

float64 :: (Monad m) => FieldParser m Double
float64 = displayValue $ liftM coerceToFloat uint64

char8 :: (Monad m) => FieldParser m Char
char8 = displayValue $ liftM (toEnum . fromIntegral) uint8

offset32 :: (Monad m) => FieldParser m Offset
offset32 = displayValue $ liftM fromIntegral int32

bool32 :: (Monad m) => FieldParser m Bool
bool32 = displayValue $ do
    v <- uint32
    when (v /= 0 && v /= 1) $ parseWarning ("unexpected boolean value: " ++ show v)
    return $ v /= 0

bytes :: (Monad m) => Int -> FieldParser m BS.ByteString
bytes l = displayValue $ do
    o <- use offset
    bs <- view inputBytes
    let r = slice (fromIntegral o) l bs
    offset += fromIntegral l
    return r

skipZeros8 :: (Monad m) => Int -> FieldParser m ()
skipZeros8 l = named "skipped bytes" . displayAs (replicate l 'b') $ do
     replicateM l $ expect 0 uint8
     return ()

skipZeros32 :: (Monad m) => Int -> FieldParser m ()
skipZeros32 l = named "skipped words" . displayAs (replicate l 'w') $ do
     replicateM l $ expect 0 uint32
     return ()

padAlignment :: (Monad m) => Offset -> FieldParser m ()
padAlignment n = do
    o <- use offset
    let padSz = n - o `mod` n
    replicateM (fromIntegral padSz) $ expect 0 uint8
    return ()

v2 :: (Monad m) => FieldParser m a -> FieldParser m (V2 a)
v2 p = liftM2 V2 p p

v3 :: (Monad m) => FieldParser m a -> FieldParser m (V3 a)
v3 p = liftM3 V3 p p p

v4 :: (Monad m) => FieldParser m a -> FieldParser m (V4 a)
v4 p = liftM4 V4 p p p p

m34 :: (Monad m) => FieldParser m a -> FieldParser m (M34 a)
m34 p = v3 (v4 p)

m33 :: (Monad m) => FieldParser m a -> FieldParser m (M33 a)
m33 p = v3 (v3 p)

m44 :: (Monad m) => FieldParser m a -> FieldParser m (M44 a)
m44 p = v4 (v4 p)


tup2 :: (Monad m) => FieldParser m a -> FieldParser m b -> FieldParser m (a, b)
tup2 p1 p2 = liftM2 (,) p1 p2

tup3 :: (Monad m) => FieldParser m a -> FieldParser m b -> FieldParser m c -> FieldParser m (a, b, c)
tup3 p1 p2 p3 = liftM3 (,,) p1 p2 p3

tup4 :: (Monad m) => FieldParser m a -> FieldParser m b -> FieldParser m c -> FieldParser m d -> FieldParser m (a, b, c, d)
tup4 p1 p2 p3 p4 = liftM4 (,,,) p1 p2 p3 p4


expect :: (Eq a, Display a, Monad m) => a -> FieldParser m a -> FieldParser m a
expect v1 p = do
    v2 <- p
    when (v1 /= v2) $ parseWarning ("Expected " ++ display v1 ++ " but found " ++ display v2)
    return v2

expectBytes :: (Monad m) => [Word8] -> FieldParser m [Word8]
expectBytes bs = expect bs $ replicateM (length bs) uint8

expectWords :: (Monad m) => [Word32] -> FieldParser m [Word32]
expectWords ws = expect ws $ replicateM (length ws) uint32

named :: (Monad m) => String -> FieldParser m a -> FieldParser m a
named n p = do
    o1 <- use offset
    (r, PO sts (SL s)) <- silence $ listen p
    o2 <- use offset
    let lbl = Label o1 (o2 - o1) (Just n)
    let sl = case s of
                 [] -> [Section lbl (Right (SL []))]
                 [Section (Label o' l' _) ss]
                   | o' == o1 && l' == o2 - o1 -> [Section lbl ss]
                 [Section _ (Left v)] -> [Section lbl (Left v)]
                 _ -> [Section lbl (Right (SL s))]
    tell $ PO sts (SL sl)
    return r

displayWith :: (Monad m) => (a -> String) -> FieldParser m a -> FieldParser m a
displayWith f p = do
    o1 <- use offset
    (r, PO sts (SL s)) <- silence $ listen p
    o2 <- use offset
    let lbl = Label o1 (o2 - o1)
    let sl = case s of
                 [Section (Label _ _ (Just n)) _] -> [Section (lbl $ Just n) (Left (f r))]
                 _ -> [Section (lbl Nothing) (Left (f r))]
    tell $ PO sts (SL sl)
    return r

displayAs :: (Monad m) => String -> FieldParser m a -> FieldParser m a
displayAs d = displayWith (const d)

displayValue :: (Display a, Monad m) => FieldParser m a -> FieldParser m a
displayValue = displayWith display

struct :: (Monad m) => String -> FieldParser m a -> StructParser m a
struct n p = ExceptT . RWST $ \i e -> do
    let o1 = i^.parserInData
    (r, st, PO strs sls) <- runRWST (runExceptT p) (i & parserInData .~ ()) (FPState e o1)
    let o2 = st^.offset
    let str = Struct o1 (o2 - o1) n sls
    return (r, st^.endianness, PO (S.insert str strs) (Last $ Just o2))

runStruct :: (Monad m) => StructParser m a -> FieldParser m a
runStruct p = ExceptT . RWST $ \i s -> do
    let o = s^.offset
    (r, e, PO strs _) <- runRWST (runExceptT p) (i & parserInData .~ o) (s^.endianness)
    let s' = s & endianness .~ e
    return $ (r, s', PO strs (SL []))

atOffset :: (Monad m) => Offset -> StructParser m a -> StructParser m a
atOffset o p = local (parserInData .~ o) p

searchForBytes :: (Monad m) => BS.ByteString -> StructParser m Offset
searchForBytes = searchFrom 0

searchForString :: (Monad m) => String -> StructParser m Offset
searchForString str = searchForBytes . BS.pack $ fromIntegral . fromEnum <$> str

searchFrom :: (Monad m) => Offset -> BS.ByteString -> StructParser m Offset
searchFrom o searchString = do
    bs <- view inputBytes
    let (pfx, _) = BS.breakSubstring searchString (BS.drop (fromIntegral o + 1) bs)
    return . fromIntegral $ BS.length pfx + 1

formatStructs :: BS.ByteString -> S.Set Struct -> [String]
formatStructs bs strs = formatStruct bs <$> fillGaps 0 (S.toList strs)
  where ln = fromIntegral $ BS.length bs
        fillGaps o [] 
            | o >= ln   = []
            | otherwise = [unusedStruct bs ln o]
        fillGaps o (s@(Struct p l _ _):ss)
            | o >= p    = s : fillGaps (p + l) ss
            | otherwise = unusedStruct bs p o  : s : fillGaps (p + l) ss

unusedStruct bs p o 
    | BS.all (== 0) bs' = Struct o (p - o) ("null (" ++ showHex (p - o) ++ " bytes)") mempty
    | otherwise         = Struct o (p - o) ("unused (" ++ showHex (p - o) ++ " bytes)") mempty
  where bs' = slice o (p - o) bs

formatStruct :: BS.ByteString -> Struct -> String
formatStruct bs (Struct p l n (SL sls)) = unlines $
    [ replicate 70 '-'
    , "| " <> n
    , "| " <> showHexPadded p <> " - " <> showHexPadded (p + l - 1)
    , formatStructBody sls
    ] ++ formatBytes o1 o2 bs
  where o1 = fromIntegral p
        o2 = fromIntegral (p + l - 1)

formatStructBody [] = []
formatStructBody sls = unlines $ "" : (formatField =<< sls)

formatField :: Section -> [String]
formatField (Section (Label o _ (Just n)) (Left v)) = [concat [showHexPadded o, "| ", n, ": ", v]]
formatField (Section (Label o _ Nothing) (Left v)) = [concat [showHexPadded o, " (unnamed) : ", v]]
formatField (Section (Label o l (Just n)) (Right (SL sls))) = header : children
  where header = concat [showHexPadded o, "| ", n, " (", showHex l, " bytes)"] 
        children = ("  " <>) <$> concatMap formatField sls
formatField (Section (Label _ _ Nothing) (Right (SL sls))) = concatMap formatField sls    

prim :: (Monad m) => Offset -> Bin.Get a -> Bin.Get a -> FieldParser m a
prim l le be = do
    st <- get
    bs <- view inputBytes
    o <- use offset
    r <- getPrimitive st le be (slice o l bs)
    offset += l
    return r

getPrimitive st le be bs = case Bin.runGetOrFail g (BSL.fromStrict bs) of
    Right (bs', _, r) | BSL.null bs' -> return r
                      | otherwise    -> parseError "runGetOrFail did not consume all input"
    Left (_, _, e) -> parseError $ "runGet: " ++ e
  where g = case st^.endianness of
                LE -> le
                BE -> be

getPrimitive' st le be bs = case Bin.runGetOrFail g (BSL.fromStrict bs) of
    Right (bs', _, r) | BSL.null bs' -> Right r
                      | otherwise    -> Left "runGetOrFail did not consume all input"
    Left (_, _, e) -> Left $ "runGet: " ++ e
  where g = case st^.endianness of
                LE -> le
                BE -> be

parseError :: (Monad m) => String -> FieldParser m a
parseError e = do
    o <- use offset
    throwError (o, e)

parseWarning :: (Monad m) => String -> FieldParser m ()
parseWarning str = do
    o <- use offset
    w <- view debugWrite
    liftFieldParser $ w o str
    return ()

liftFieldParser :: (Monad m) => m a -> FieldParser m a
liftFieldParser = lift . lift

liftStructParser :: (Monad m) => m a -> StructParser m a
liftStructParser = lift . lift

{-
--  dummy :: (Monad m) => Offset -> String -> FieldParser m String
dummy :: (Show a, Monad m) => Bin.Get a -> Offset -> String -> FieldParser m String
dummy f l v = displayValue $ do
    st <- get
    bs <- view inputBytes
    o <- use offset
    --  parseWarning $ show (o, l)
    --  parseWarning . show $ (bs, slice o l bs)
    let r = getPrimitive' st f f (slice o l bs)
    parseWarning $ show r
    --  r <- case getPrimitive' st f f (slice o l bs) of
        --  Right pr -> return pr
        --  Left err -> parseError err
    --  r' <- getPrimitive st f f (slice o l bs)
    offset += l
    return v

ptest = do
    _ <- struct "test1" 0 $ do
        --  replicateM 4 char8
        replicateM 4 (named "2-byte field" $ dummy Bin.getWord16le 2 "--")
    _ <- struct "test2" 8 $ do
        _ <- named "4-byte field" $ dummy Bin.getWord32le 4 "????"
        _ <- runStruct . struct "test3" 0x20 $ do
            named "1-byte field" $ dummy Bin.getWord8 1 "*"
            named "3 byte padding" $ bytes 0x03 -- dummy 3 "..."
            named "8-byte field" $ dummy Bin.getWord64le 8 "---- ----"
        _ <- named "compound field" $ liftM unlines (replicateM 2 $ named "4-byte field" $ dummy Bin.getWord32le 4 "----")

        _ <- named "raw bytes" $ bytes 0x08
        return ()
    return ()
-}
