module FP.Task2.Type
  (
    Arc
  , Graph
  , DijkstraState (..)
  ) where

import           Data.Map.Strict (Map)

type Arc = (String, Integer)

type Graph = String -> [Arc]

data DijkstraState = DijkstraState { stDist    :: Map String Integer
                                   , stVisited :: Map String Bool
                                   }

