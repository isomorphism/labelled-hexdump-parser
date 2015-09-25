{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Util where

import qualified Numeric
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.RWS.Class
import Data.List.Split (chunksOf)
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import Data.Int
import Data.Word
import Data.List ((!!))
import Data.Bits
import Data.Bits.Lens
import Linear
import Overture
import Prelude ()

class Display a where
    display :: a -> String
    
    default display :: (Show a) => a -> String
    display = show

instance Display Word8
instance Display Word16
instance Display Word32
instance Display Word64
instance Display Int8
instance Display Int16
instance Display Int32
instance Display Int64
instance Display Int
instance Display Float
instance Display Double

instance Display Bool
instance Display Integer

instance Display Rational where
    display n = show (realToFrac n :: Float)

instance Display Char where
    display = return

instance Display String where
    display = id

instance Display BS.ByteString where
    display = intercalate "\n" 
            . ([""] ++) 
            . map ((replicate 8 ' ' ++) . unwords)
            . chunksOf 0x10 
            . map formatByte 
            . BS.unpack

instance (Display a, Display b) => Display (a, b) where
    display (a, b) = concat [ "(", display a, ", ", display b, ")"]

instance (Show a) => Display (V2 a)
instance (Show a) => Display (V3 a)
instance (Show a) => Display (V4 a)

instance Display [Word8]
instance Display [Word32]


convertEnum e = if enumIsBetween mn mx e then Just r else Nothing
  where [mn, mx, _] = [minBound, maxBound, r]
        r = toEnum e

enumIsBetween :: (Enum a) => a -> a -> Int -> Bool
enumIsBetween a z x = x >= fromEnum a && x <= fromEnum z


showHex n | n >= 0    = "0x" ++ Numeric.showHex n ""
          | otherwise = "-" ++ showHex (-n)

showHexPadded n 
    | n >= 0    = "0x" ++ pad ++ s
    | otherwise = "-" ++ showHexPadded (-n)
  where s = Numeric.showHex n ""
        pad = replicate (max 0 $ 8 - length s) '0'



readBinary :: (Num a, Bits a) => String -> a
readBinary b = zeroBits & bits .@~ ((map (/= '0') (reverse b) ++ repeat False) !!)

showBinary :: (Num a, Bits a) => a -> String
showBinary b = (\i -> if i then '1' else '0') <$> toListOf bits b

showAbsOffset (-1) = "(INVALID) "
showAbsOffset o = showHexPadded o

formatBytes :: Int -> Int -> BS.ByteString -> [String]
formatBytes o1 o2 _ | o2 < o1 = []
formatBytes o1 o2 bs 
    | BS.all (== 0) bs' = ["..."]
    | otherwise         = zipWith formatLine offsets byteLines
  where l = 1 + o2 - o1
        b' = formatByte <$> take l (drop o1 b)
        prefixSz = 16 - o1 `mod` 16
        firstLine = replicate (16 - prefixSz) "  " ++ take prefixSz b'
        otherLines = chunksOf 16 $ drop prefixSz b'
        byteLines = firstLine : otherLines
        firstOffset = o1 - o1 `mod` 16
        offsets = [firstOffset, firstOffset + 0x10 .. o2]
        b = BS.unpack bs
        bs' = slice o1 l bs

formatLine :: Int -> [String] -> String
formatLine o l = concat [showHexPadded o, "| ", w1, "  ", w2, "   ", w3, "  ", w4, "  ", printedChars]
    where l' = chunksOf 4 l ++ repeat (replicate 4 "  ")
          (w1:w2:w3:w4:_) = intercalate " " <$> l'
          --  printedChars = chunksOf 4 $ showPrintableChar <$> unwords [w1, w2, w3, w4]
          printedChars = showPrintableChar <$> concat (take 4 l')

formatVertIxs :: (Show a) => [a] -> String
formatVertIxs = unlines . map (intercalate " ") . chunksOf 3 . map show . chunksOf 3

showPrintableChar :: String -> Char
showPrintableChar "" = ' '
showPrintableChar " " = ' '
showPrintableChar "  " = ' '
showPrintableChar s 
    | c >= 32 && c < 127 = toEnum c
    | otherwise          = '.'
  where c = case Numeric.readHex s of
                [(h, "")] -> h
                _ -> 0

formatByte b 
    | b < 0x10  = "0" ++ Numeric.showHex b ""
    | otherwise = Numeric.showHex b ""

slice :: (Integral a) => a -> a -> BS.ByteString -> BS.ByteString
slice o l = BS.take (fromIntegral l) . BS.drop (fromIntegral o)

breakNybbles :: Word8 -> [Word8]
breakNybbles w = [w^.nybbleAt 1, w^.nybbleAt 0]

pack16le :: [Word8] -> [Word16]
pack16le = map (Bin.runGet Bin.getWord16le . BSL.pack) . chunksOf 2

pack32le :: [Word8] -> [Word32]
pack32le = map (Bin.runGet Bin.getWord32le . BSL.pack) . chunksOf 4


btAt :: (Integral a, Num a, Bits a) => Int -> Int -> Lens' a Word8
btAt n = \p -> lens (btGet n p) (btSet n p)

btMask :: (Bits a) => Int -> a
btMask n = foldl' setBit zeroBits [0..n - 1]

btGet :: (Integral a, Num a, Bits a) => Int -> Int -> a -> Word8
btGet n p w = fromIntegral $ w `shiftR` p .&. btMask n

btSet :: (Integral a, Num a, Bits a) => Int -> Int -> a -> Word8 -> a
btSet n p w v = w .&. invMask .|. v'
  where ix' = p
        v' = fromIntegral (v .&. btMask n) `shiftL` ix'
        invMask = complement $ btMask n `shiftL` ix'

nybbleAt :: (Integral a, Num a, Bits a) => Int -> Lens' a Word8
nybbleAt ix = btAt 4 (ix * 4)




silence :: (MonadWriter w m) => m a -> m a
silence = censor (const mempty)
