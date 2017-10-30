{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Language.EDN
  ( EDN
  , int
  , float
  , symbol
  , listOf
  , mapOf
  , vectorOf
  , setOf
  , key
  , keyAt
  , _Boolean
  , _Character
  , _Float
  , _Integer
  , _Keyword
  , _List
  , _Map
  , _Set
  , _String
  , _Symbol
  , _TagName
  , _TagVal
  , _Vector
  , _Seq
  ) where

import Control.Lens (Prism', Traversal', at, ix, preview, prism')
import Data.Foldable (toList)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Text (Text, pack)
import qualified Data.Vector as V
import Data.Vector.Instances ()
import qualified GHC.Exts as Exts
import GHC.Generics (Generic)
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (KnownSymbol, symbolVal')

data EDN
  = Nil
  | Boolean Bool
  | Character Char
  | Float Float
  | Integer Int
  | Keyword Text
  | List [EDN]
  | Map (HashMap EDN EDN)
  | Set (HashSet EDN)
  | String Text
  | Symbol Text
  | Tag Text EDN
  | Vector (V.Vector EDN)
  deriving (Show, Eq, Generic)

instance Hashable EDN

instance IsString EDN where
  fromString = String . pack

instance KnownSymbol s => IsLabel s EDN where
  fromLabel p = Keyword . pack $ symbolVal' p

instance KnownSymbol s => IsLabel s (EDN -> EDN) where
  fromLabel p = Tag . pack $ symbolVal' p

instance Exts.IsList EDN where
  type Item EDN = EDN
  fromList = List
  toList = fromMaybe [] . preview _Seq

int :: Int -> EDN
int = Integer

float :: Float -> EDN
float = Float

symbol :: Text -> EDN
symbol = Symbol

listOf :: [EDN] -> EDN
listOf = List

vectorOf :: [EDN] -> EDN
vectorOf = Vector . V.fromList

setOf :: [EDN] -> EDN
setOf = Set . HashSet.fromList

mapOf :: [EDN] -> EDN
mapOf = Map . HashMap.fromList . pairs
 where
  pairs [] = []
  pairs (k:v:xs) = (k, v):pairs xs
  pairs [k] = [(k, Nil)]

key :: EDN -> Traversal' EDN EDN
key k = _Map . ix k

keyAt :: EDN -> Traversal' EDN (Maybe EDN)
keyAt k = _Map . at k

_Boolean :: Prism' EDN Bool
_Boolean = prism' Boolean $ \case
  Boolean x -> Just x
  _ -> Nothing

_Character :: Prism' EDN Char
_Character = prism' Character $ \case
  Character xs -> Just xs
  _ -> Nothing

_Float :: Prism' EDN Float
_Float = prism' Float $ \case
  Float xs -> Just xs
  _ -> Nothing

_Integer :: Prism' EDN Int
_Integer = prism' Integer $ \case
  Integer xs -> Just xs
  _ -> Nothing

_Keyword :: Prism' EDN Text
_Keyword = prism' Keyword $ \case
  Keyword xs -> Just xs
  _ -> Nothing

_List :: Prism' EDN [EDN]
_List = prism' List $ \case
  List xs -> Just xs
  _ -> Nothing

_Map :: Prism' EDN (HashMap EDN EDN)
_Map = prism' Map $ \case
  Map xs -> Just xs
  _ -> Nothing

_Set :: Prism' EDN (HashSet EDN)
_Set = prism' Set $ \case
  Set xs -> Just xs
  _ -> Nothing

_String :: Prism' EDN Text
_String = prism' String $ \case
  String xs -> Just xs
  _ -> Nothing

_Symbol :: Prism' EDN Text
_Symbol = prism' Symbol $ \case
  Symbol xs -> Just xs
  _ -> Nothing

_TagName :: Traversal' EDN Text
_TagName f edn =
  case edn of
    Tag n v -> flip Tag v <$> f n
    _ -> pure edn

_TagVal :: Traversal' EDN EDN
_TagVal f edn =
  case edn of
    Tag n v -> Tag n <$> f v
    _ -> pure edn

_Vector :: Prism' EDN (V.Vector EDN)
_Vector = prism' Vector $ \case
  Vector xs -> Just xs
  _ -> Nothing

_Seq :: Traversal' EDN [EDN]
_Seq f edn =
  case edn of
    List xs -> List <$> f xs
    Vector xs -> List <$> f (toList xs)
    Set xs -> List <$> f (toList xs)
    Map ms -> List <$> f (toList ms)
    _ -> pure edn
