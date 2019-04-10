module FP.Task1.Type
  (
    Trie (..)
  , Dictionary
  ) where

import qualified Data.Map.Strict as M

data Trie a = Node (Maybe a) (M.Map Char (Trie a))
  deriving (Show, Eq)

type Dictionary = Trie String
