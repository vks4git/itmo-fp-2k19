{-# LANGUAGE RecordWildCards #-}

module FP.Task2.Util
  (
    distance
  ) where

import           Data.List       (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust, fromMaybe)
import           FP.Task2.Type   (DijkstraState (..), Graph)

distance :: Graph -> String -> String -> Maybe Integer
distance graph u v = dist v resultingDistances
  where
    initialDistances = M.singleton u 0
    initialVisited   = M.empty
    initialState     = DijkstraState initialDistances initialVisited

    finalState = dijkstraAlgorithm graph initialState
    resultingDistances = stDist finalState

dijkstraAlgorithm :: Graph -> DijkstraState -> DijkstraState
dijkstraAlgorithm graph ds@DijkstraState {..} | null keys = ds
                                              | otherwise = dijkstraAlgorithm graph newState
  where
    keys     = filter (not . visited stVisited) . fmap fst . sortOn snd $ M.assocs stDist
    newState = foldl (dijkstraIteration graph) ds keys

dijkstraIteration :: Graph -> DijkstraState -> String -> DijkstraState
dijkstraIteration graph ds v | visited (stVisited ds) v = ds
                             | otherwise = ds { stVisited = visit v (stVisited ds)
                                              , stDist    = updateDist (stDist ds)
                                              }
  where
    incident = graph v

    updateDist :: Map String Integer -> Map String Integer
    updateDist dmap = foldl updateVert dmap incident

    updateVert :: Map String Integer -> (String, Integer) -> Map String Integer
    updateVert dmap (u, weight) = if dist u dmap `greater` fmap (+ weight) (dist v dmap)
                                  then setDist u dmap (fromJust (dist v dmap) + weight)
                                  else dmap


visited :: Map String Bool -> String -> Bool
visited vmap key = fromMaybe False $ M.lookup key vmap

visit :: String -> Map String Bool -> Map String Bool
visit key = M.insert key True

dist :: String -> Map String Integer -> Maybe Integer
dist = M.lookup

greater :: Maybe Integer -> Maybe Integer -> Bool
greater Nothing (Just _)  = True
greater (Just a) (Just b) = a > b
greater _ _               = False

setDist :: String -> Map String Integer -> Integer -> Map String Integer
setDist key distMap distVal = M.insert key distVal distMap
