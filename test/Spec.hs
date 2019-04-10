import           Data.List       (delete, sort)
import           Data.Map.Strict (fromList, (!))
import qualified Data.Map.Strict as M
import           FP.Task1        (Dictionary, empty, foldd, get, put, remove)
import           FP.Task1.Type   (Trie (..))
import           FP.Task2        (Graph, distance)
import           Test.Hspec

main :: IO ()
main = hspec $ do
   task1tests
   task2tests


-----------------------------------------------------------------------------------------------------------------------
  -- Task 1 tests
-----------------------------------------------------------------------------------------------------------------------


task1tests :: Spec
task1tests = do
  describe "Insertion" $ do
    it "Insertion into empty trie"      $ put "flag" "flag value" empty            `shouldBe` (Nothing, dictWithFlag)
    it "Insertion into non-empty trie"  $ put "flux" "flux value" dictWithFlag     `shouldBe` (Nothing, dictWithFlagFlux)
    it "Insertion of a present element" $ put "flag" "new value"  dictWithFlagFlux `shouldBe` (Just "flag value", dictWithFlagFlux')
  describe "Query" $ do
    it "Query from an empty trie"    $ get "foo"     empty    `shouldBe` Nothing
    it "Querying a present element"  $ get "bar"     testDict `shouldBe` Just "bar value"
    it "Querying a present element"  $ get "fort"    testDict `shouldBe` Just "fort value"
    it "Querying a present element"  $ get "foolish" testDict `shouldBe` Just "foolish value"
    it "Querying a present element"  $ get "settler" testDict `shouldBe` Just "settler value"
    it "Querying an absent element"  $ get "quux"    testDict `shouldBe` Nothing
    it "Querying an absent element"  $ get "fortify" testDict `shouldBe` Nothing
    it "Querying an absent element"  $ get "settle"  testDict `shouldBe` Nothing
    it "Querying an absent element"  $ get "sets"    testDict `shouldBe` Nothing
  describe "Deletion" $ do
    it "Deletion from an empty trie"   $ remove "fort"      empty    `shouldBe` (Nothing, empty)
    it "Deletion of a present element" $ remove "fortalesa" testDict `shouldBe` (Just "fortalesa value", testDictWoFortalesa)
    it "Deletion of an absent element" $ remove "settle"    testDict `shouldBe` (Nothing, testDict)
  describe "Folding" $ do
    it "Folding of an empty trie"    $ foldd (:) [] empty `shouldBe` []
    it "Folding of a non-empty trie" $ foldd ((:) . fst) [] testDict `shouldBe` sort wordList

testDict :: Dictionary
testDict = mkDict wordList

testDictWoFortalesa :: Dictionary
testDictWoFortalesa = mkDict $ delete "fortalesa" wordList

-- | Dictionary with key "flag" and its value "flag value"
--
dictWithFlag :: Dictionary
dictWithFlag = Node Nothing (fromList [('f',
                 Node Nothing (fromList [('l',
                   Node Nothing (fromList [('a',
                     Node Nothing (fromList [('g',
                       Node (Just "flag value") M.empty)
                                            ])
                                           )])
                                         )])
                                       )])

-- | Dictionary with records ("flag", "flag value") and ("flux", "flux value")
--
dictWithFlagFlux :: Dictionary
dictWithFlagFlux = Node Nothing (fromList [('f',
                     Node Nothing (fromList [('l',
                       Node Nothing (fromList [('a',
                         Node Nothing (fromList [('g',
                           Node (Just "flag value") M.empty)]))
                                             , ('u',
                         Node Nothing (fromList [('x',
                           Node (Just "flux value") M.empty)
                                                ])
                                              )])
                                             )])
                                           )])

-- | Dictionary with records ("flag", "new value") and ("flux", "flux value")
--
dictWithFlagFlux' :: Dictionary
dictWithFlagFlux' = Node Nothing (fromList [('f',
                      Node Nothing (fromList [('l',
                        Node Nothing (fromList [('a',
                          Node Nothing (fromList [('g',
                            Node (Just "new value") M.empty)]))
                                              , ('u',
                          Node Nothing (fromList [('x',
                            Node (Just "flux value") M.empty)
                                                 ])
                                               )])
                                              )])
                                            )])

mkDict :: [String] -> Dictionary
mkDict = foldl foldPut empty . mkKeys
  where
    foldPut :: Dictionary -> (String, String) -> Dictionary
    foldPut d (k, v) = snd $ put k v d


mkKeys :: [String] -> [(String, String)]
mkKeys = fmap (\key -> (key, key <> " value"))

wordList :: [String]
wordList = [ "foo", "fortalesa", "forest", "foreign", "fortnight", "fort", "foolish"
           , "bar", "bag", "barostate", "barometer", "bargain", "barracks"
           , "set", "selection", "setting", "setosa", "settler", "settlement"
           ]


-----------------------------------------------------------------------------------------------------------------------
  -- Task 2 tests
-----------------------------------------------------------------------------------------------------------------------


task2tests :: Spec
task2tests = do
  describe "Connected graph" $ do
    it "Incident vertices"  $ distance graph1 "v1" "v2" `shouldBe` Just 2
    it "Connected vertices" $ distance graph1 "v1" "v9" `shouldBe` Just 11
  describe "Disconnected graph" $ do
    it "Incident vertices"    $ distance graph2 "v6" "v4" `shouldBe` Just 1
    it "Connected vertices"   $ distance graph2 "v1" "v7" `shouldBe` Just 7
    it "Connected vertices"   $ distance graph2 "v8" "v9" `shouldBe` Just 5
    it "Unreachable vertices" $ distance graph2 "v6" "v8" `shouldBe` Nothing


graph1 :: Graph
graph1 = mkGraph edges1

graph2 :: Graph
graph2 = mkGraph edges2

mkGraph :: [(String, [(String, Integer)])] -> Graph
mkGraph incList = (fromList incList !)


-- v1 --2-- v2 --4-- v3
-- |        |      /
-- 3        5    6
-- |        |  /
-- v6       v4 --1-- v5
-- |      /        / |
-- 4    7        2   3
-- |  /        /     |
-- v7 --9-- v8 --7-- v9
--
edges1 :: [(String, [(String, Integer)])]
edges1 = [ ("v1", [("v2", 2), ("v6", 3)])
         , ("v2", [("v1", 2), ("v3", 4), ("v4", 5)])
         , ("v3", [("v2", 4), ("v4", 6)])
         , ("v4", [("v2", 5), ("v3", 6), ("v5", 1), ("v7", 7)])
         , ("v5", [("v4", 1), ("v8", 2), ("v9", 3)])
         , ("v6", [("v1", 3), ("v7", 4)])
         , ("v7", [("v4", 7), ("v6", 4), ("v8", 9)])
         , ("v8", [("v5", 2), ("v7", 9), ("v9", 7)])
         , ("v9", [("v5", 3), ("v8", 7)])
         ]


-- v1 --2-- v2 --4-- v3
-- |        |      /
-- 3        5    6
-- |        |  /
-- v6 --1-- v4       v5
-- |      /        / |
-- 4    7        2   3
-- |  /        /     |
-- v7       v8 --7-- v9
--
edges2 :: [(String, [(String, Integer)])]
edges2 = [ ("v1", [("v2", 2), ("v6", 3)])
         , ("v2", [("v1", 2), ("v3", 4), ("v4", 5)])
         , ("v3", [("v2", 4), ("v4", 6)])
         , ("v4", [("v2", 5), ("v3", 6), ("v6", 1), ("v7", 7)])
         , ("v5", [("v8", 2), ("v9", 3)])
         , ("v6", [("v1", 3), ("v4", 1), ("v7", 4)])
         , ("v7", [("v4", 7), ("v6", 4)])
         , ("v8", [("v5", 2), ("v9", 7)])
         , ("v9", [("v5", 3), ("v8", 7)])
         ]

