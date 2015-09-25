module Common where

import Parser
import Util
import Types

import Overture
import Prelude ()


{---------------------------------------
-- Dictionary (collection of structs with named entries)
--
-- base fields: count, dict offset
-- @ dict offset: dict header struct, dict entry structs (# of times = count)
-- each dict entry contains struct offsets
-}

parseDict :: (Monad m) => String -> StructParser m a -> FieldParser m (Dict a)
parseDict n p = named (n ++ " dict") $ do
    _ <- named "# of entries" int
    o <- named "dict offset" relOffset
    if o > 0 then runStruct . atOffset o $ parseDictHeader n p
             else return $ Dict 0 (DictNode 0 0 0 "" Nothing) []

parseDictHeader :: (Monad m) => String -> StructParser m a -> StructParser m (Dict a)
parseDictHeader n p = struct ("DICT (" ++ n ++ ")") $ do
    expectSig "DICT"
    _ <- named "size" uint32
    eCnt <- named "# of entries" int
    rnode <- parseDictRootNode p
    nodes <- replicateM eCnt $ parseDictNode p
    return $ Dict eCnt rnode nodes

parseDictRootNode p = named "DICT root node" $ do
    rb <- named "ref bit" int32
    li <- named "left index" uint16
    ri <- named "right index" uint16
    n <- named "node name" symbol
    o <- named "node data offset" relOffset
    v <- if o >= 0 then liftM Just . runStruct $ atOffset o p else return Nothing
    return $ DictNode rb li ri n v

parseDictNode p = named "DICT node" $ do
    rb <- named "ref bit" int32
    li <- named "left index" uint16
    ri <- named "right index" uint16
    n <- named "node name" symbol
    o <- named "node data offset" relOffset
    v <- runStruct $ atOffset o p
    return $ DictNode rb li ri n v

{- ---------------------------------------
-- List (sequence of structs)
--
-- base fields: count, list offset
-- @ list offset: offsets (# of times = count)
-- @ each list offset: some struct
-}

parseList :: (Monad m) => String -> StructParser m a -> FieldParser m [a]
parseList n p = named (n ++ " list") $ do
    cnt <- named "# of entries" int
    o <- named "list offset" relOffset
    if o > 0 then runStruct . atOffset o $ do parseListEntries cnt n p
             else return $ []

parseListEntries :: (Monad m) => Int -> String -> StructParser m a -> StructParser m [a]
parseListEntries cnt n p = struct ("list (" ++ n ++ ")") $ do
    ofs <- replicateM cnt relOffset
    mapM (runStruct . flip atOffset p) ofs

{- ---------------------------------------
-- Array (sequence of values)
--
-- base fields: count, array offset
-- @ array offset: values (# of times = count)
-}

parseArray :: (Monad m) => String -> FieldParser m a -> FieldParser m [a]
parseArray n p = named (n ++ " array") $ do
    cnt <- named "# of entries" int
    o <- named "array offset" relOffset
    if o > 0 then runStruct . atOffset o $ do parseArrayEntries cnt n p
             else return $ []

parseArrayEntries :: (Monad m) => Int -> String -> FieldParser m a -> StructParser m [a]
parseArrayEntries cnt n p = struct ("array (" ++ n ++ ")") $ replicateM cnt p


symbol :: (Monad m) => FieldParser m String
symbol = displayWith id $ do
    o <- relOffset
    if o > 0 then runStruct $ atOffset o parseString
             else return ""

parseString :: (Monad m) => StructParser m String
parseString = struct "symbol" $ 
    displayWith id . named "string" $ repeatUntil (== '\0') char8

--  repeatUntil :: (Monad m) => (a -> Bool) -> FieldParser m a -> FieldParser m [a]
repeatUntil f p = do
    r <- p
    if f r then return [] 
           else liftM (r:) $ repeatUntil f p

expectSig :: (Monad m) => String -> FieldParser m ()
expectSig str1 = do
    named "signature" . displayAs str1 . expect str1 $ replicateM 4 char8
    return ()

relOffset :: (Monad m) => FieldParser m Offset
relOffset = displayWith showAbsOffset $ do
    o <- use offset
    r <- offset32
    return $ if r /= 0 then o + r else -1

parseUnknown :: (Monad m) => String -> StructParser m ()
parseUnknown n = do
    struct n $ do
        parseWarning $ "unknown structure : " ++ n
        return ()

