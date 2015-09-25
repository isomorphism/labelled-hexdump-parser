{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
module Types where

import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.RWS.Class
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import Data.Bits
import Data.Bits.Lens
import Data.Bits.Floating
import Data.IORef
import Data.Word
import Data.Int
import Data.List.Split (chunksOf)
import Linear
import Codec.Picture
import qualified Numeric (showHex)

import Parser
import Util

import Overture
import Prelude ()

newtype Signature = Sig String
  deriving (Eq, Ord, Read, Show)

newtype Bitflags = Bitflags { fromBitflags :: Word32 }
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show, Bits)

instance Display Bitflags where
    display = unwords . chunksOf 4 . concatMap show . toListOf (bits.from enum) . fromBitflags

type Version = [Word8]

data Dict a = Dict
    { _dictEntryCount :: Int
    , _rootNode :: DictNode (Maybe a)
    , _dictEntries :: [DictNode a]
    } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data DictNode a = DictNode
    { _refBit :: Int32
    , _lIndex :: Word16
    , _rIndex :: Word16
    , _nodeName :: String 
    , _nodeData :: a
    } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

makeLenses ''Dict
makeLenses ''DictNode

dictValues :: Traversal' (Dict a) a
dictValues = dictEntries . each . nodeData


type instance Index (Dict a) = String
type instance IxValue (Dict a) = a
instance Ixed (Dict a) where
    ix k f dict = fmap (($ dict) . (dictEntries .~)) $ for (dict^.dictEntries) $ \e ->
        if e^.nodeName == k 
        then ($ e) . (nodeData .~) <$> f (e^.nodeData) 
        else pure e



