import Data.List

data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix a b = a == take x b
   where
     x = length a

removePrefix :: String -> String -> String
removePrefix a b = drop x b
   where
     x = length a

suffixes :: [a] -> [[a]]
suffixes a = take x (iterate tail a)
   where
     x = length a

isSubstring :: String -> String -> Bool
isSubstring a b = or (map (isPrefix a) (suffixes b))

findSubstrings :: String -> String -> [Int]
findSubstrings a b = elemIndices True (map (isPrefix a) (suffixes b))

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Node []) = []
getIndices (Node a) = getIndices' a
   where
     getIndices' :: [(String, SuffixTree)] -> [Int]
     getIndices' [] = []
     getIndices' ((_, (Leaf x)) : as) = x : getIndices' as
     getIndices' ((_, (Node x)) : as) = getIndices' x ++ getIndices' as

-- ' added since i imported data.list which causes confusion
partition' :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition' a b = (take x a, drop x a, drop x b)
   where
     x = partition'' (take y a) (take y b) y
     y = min (length a) (length b)
     partition'' :: Eq a => [a] -> [a] -> Int -> Int
     partition'' [] [] _ = 0
     partition'' a b y
        | a == b = y
        | otherwise = partition'' (take y' a) (take y' b) y'
           where
             y' = y - 1

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Leaf _) = []
findSubstrings' s (Node xs)
  = concat [getIndices t | (a, t) <- xs, isPrefix s a] ++
    concat [findSubstrings' (removePrefix a s) t | (a, t) <- xs, isPrefix a s]

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert
  = undefined

-- This function is given
buildTree :: String -> SuffixTree
buildTree s
  = foldl (flip Main.insert) (Node []) (zip (suffixes s) [0..length s-1])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1
  = "banana"

s2 :: String
s2
  = "mississippi"

t1 :: SuffixTree
t1
  = Node [("banana", Leaf 0),
          ("a", Node [("na", Node [("na", Leaf 1),
                                   ("", Leaf 3)]),
                     ("", Leaf 5)]),
          ("na", Node [("na", Leaf 2),
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2
  = Node [("mississippi", Leaf 0),
          ("i", Node [("ssi", Node [("ssippi", Leaf 1),
                                    ("ppi", Leaf 4)]),
                      ("ppi", Leaf 7),
                      ("", Leaf 10)]),
          ("s", Node [("si", Node [("ssippi", Leaf 2),
                                   ("ppi", Leaf 5)]),
                      ("i", Node [("ssippi", Leaf 3),
                                  ("ppi", Leaf 6)])]),
          ("p", Node [("pi", Leaf 8),
                      ("i", Leaf 9)])]
