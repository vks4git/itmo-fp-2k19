module FP.Task1.Type
  (
    Trie (..)
  , Dictionary
  ) where


import qualified Data.IntMap.Strict as M

data Trie a = Node (Maybe a) (M.IntMap (Trie a))
  deriving (Show, Eq)

type Dictionary = Trie String
