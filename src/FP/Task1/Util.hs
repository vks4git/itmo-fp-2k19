{-# LANGUAGE ScopedTypeVariables #-}

module FP.Task1.Util
  (
    get
  , put
  , remove
  , foldd
  , empty
  ) where

import qualified Data.Map.Strict as M
import           FP.Task1.Type   (Dictionary, Trie (..))

-- | An empty dictionary.
--
empty :: Dictionary
empty = Node Nothing M.empty

-- | Get a value from the dictionary by its key.
--
get :: String -> Dictionary -> Maybe String
get [] (Node maybeVal _) = maybeVal
get (x:xs) (Node _ imap) = case M.lookup x imap of
                              Just tree -> get xs tree
                              _         -> Nothing

-- | Insert a key-value pair into the dictionary.
-- Return old value, if exisis, and the updated dictionary.
--
put :: String -> String -> Dictionary -> (Maybe String, Dictionary)
put [] val (Node oldVal imap) = (oldVal, Node (Just val) imap)
put (x:xs) val (Node v imap)  = case M.lookup x imap of
                                  Nothing   -> (Nothing, Node v $ M.insert x (snd $ put xs val empty) imap)
                                  Just dict -> let (oldVal, newDict) = put xs val dict
                                               in  (oldVal, Node v $ M.insert x newDict imap)

-- | Remove a key from the dictionary.
-- Return removed value, if exists, and the updated dictionary.
--
remove :: String -> Dictionary -> (Maybe String, Dictionary)
remove [] (Node oldVal imap) = (oldVal, Node Nothing imap)
remove (x:xs) (Node v imap)  = case M.lookup x imap of
                                 Nothing   -> (Nothing, Node v imap)
                                 Just dict -> let (val, newDict@(Node _ newImap)) = remove xs dict
                                              in if M.null newImap
                                                 then (val, Node v $ M.delete x imap)
                                                 else (val, Node v $ M.insert x newDict imap)

-- | Fold a dictionary traversing keys in descending order.
--
foldd :: ((String, String) -> a -> a) -> a -> Dictionary -> a
foldd = prefixFoldd []

prefixFoldd :: forall a. String
            -> ((String, String) -> a -> a)
            -> a
            -> Dictionary
            -> a
prefixFoldd prefix fun initV (Node (Just val) imap) = fun (prefix, val) $ prefixFoldd prefix fun initV (Node Nothing imap)
prefixFoldd prefix fun initV (Node Nothing imap)    = foldr foldingFun initV keyAssocs
  where
    assocs :: [(Char, Dictionary)]
    assocs = M.toAscList imap

    keyAssocs :: [(String, Dictionary)]
    keyAssocs = (\(a, b) -> ((prefix <>) . pure $ a, b)) <$> assocs

    foldingFun :: (String, Dictionary) -> a -> a
    foldingFun (newPrefix, dict) initV' = prefixFoldd newPrefix fun initV' dict


