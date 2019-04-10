{-# LANGUAGE TemplateHaskell #-}

module FP.Task2.Type
  (
    Arc
  , Graph
  , DijkstraState (..)
  , stDist
  , stVisited
  ) where

import           Control.Lens    (makeLenses)
import           Data.Map.Strict (Map)

type Arc = (String, Integer)

type Graph = String -> [Arc]

data DijkstraState = DijkstraState { _stDist    :: Map String Integer
                                   , _stVisited :: Map String Bool
                                   }
makeLenses ''DijkstraState

