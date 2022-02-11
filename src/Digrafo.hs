module Digrafo (
    Digrafo (G), -- sólo se exporta el tipo, pero no sus constructores
    vertices,
    arcos,
    nVertices,
    nArcos,
    sucesores,
    antecesores,
    gradoSal,
    gradoEnt,
    depthFirstSearch,
    topologicalSort)
where

import Data.List (foldl',scanl', find, sort, groupBy, nub)
import Data.Function (on)
import Data.Maybe (maybeToList)
import Control.Monad(guard,forM)
import Debug.Trace -- TODO: DELETE

import qualified Control.Monad.State as S

-----------------------
-- | TODO: Move to test suite
grafo1 = (G [1..4] suc)
    where suc 1 = [2,3]
          suc 2 = [4]
          suc 3 = [4]
          suc 4 = []

grafo2 = (G [1..10] next)
    where next x
            | x /= 10   = return . succ $ x
            | otherwise = return 1

grafo3 = (G [1..10] next)
    where next  1 = [3]
          next  2 = [1]
          next  3 = [4,5]
          next  4 = [6]
          next  5 = [6]
          next  6 = [2,7]
          next  7 = [2,8,9]
          next  8 = [9]
          next  9 = []
          next 10 = []

-- from Algorithms 4th Ed. Sedgewick (page 576)
grafo4 = (G [0..12] next)
    where next  0 = [5,1,6]
          next  1 = []
          next  2 = [3,0]
          next  3 = [5]
          next  4 = []
          next  5 = [4]
          next  6 = [4,9]
          next  7 = [6]
          next  8 = [7]
          next  9 = [11,12,10]
          next 10 = []
          next 11 = [12]
          next 12 = []

grafo5 = (G ['a'..'h'] next)
    where next 'a' = "fd"
          next 'b' = "de"
          next 'c' = "e"
          next 'd' = "h"
          next 'e' = "g"
          next 'f' = "g"
          next 'g' = ""
          next 'h' = "" 
        
grafo6 = (G vs next)
    where vs = ["belt","jacket","pants","shirt","socks","shoes","tie","undershorts","watch"]
          next "belt" = ["jacket"]
          next "jacket" = []
          next "pants" = ["belt","shoes"]
          next "shirt" = ["belt","tie"]
          next "socks" = ["shoes"]
          next "shoes" = []
          next "tie" = ["jacket"]
          next "undershorts" = ["pants","shoes"]
          next "watch" = []

t = trans grafo1
vs = vertices grafo1
-----------------------


data Digrafo v = G [v] (v -> [v])

vertices :: Digrafo v -> [v]
vertices (G vs _) = vs

-- utility function
-- | returns the transition/succesor function
trans (G _ t) = t


duplicate a = (a,a)

-- utility function
-- | Takes a transition function and runs it over each element of the given list.
--
-- returns 
runTransition :: (a -> b) -> [a] -> [(a, b)]
runTransition transition = fmap (fmap transition . duplicate)


-- TODO: Simplify
arcos :: Digrafo v -> [(v, v)]
arcos g = mergePairs arcsByRoot
    where arcsByRoot = runTransition (trans g) (vertices g)
          mergePairs = concat . fmap (\(root,nexts) -> fmap ((,) root) nexts) -- it's not clear!
          
    -- what to do?
    --  for every vertex, find the successor
    --  so, we can have something like this:
    --      1 -> [2,3]
    --          implies:
    --              (1,2), (2,3)
    --      2 -> [4]
    --          implies:
    --              (2,4)
    --     ...
    --
    --     so, we can construct this with the following function:
    --     fmap (sucessor . duplicate) . vertices
    --      -> 
    --
    --     fmap (\x) [1..4]

nVertices :: Digrafo v -> Int
nVertices = length . vertices

nArcos :: Digrafo v -> Int
nArcos = length . arcos


sucesores :: Eq v => Digrafo v -> v -> [v]
sucesores g v = fmap snd . filter ((== v) . fst) . arcos $ g

antecesores :: Eq v => Digrafo v -> v -> [v]
antecesores g v = fmap fst . filter ((== v) . snd) . arcos $ g

gradoSal :: Eq v => Digrafo v -> v -> Int
gradoSal g = length . sucesores g

gradoEnt :: Eq v => Digrafo v -> v -> Int
gradoEnt g = length . antecesores g


-- | Pre-requisites:
--
-- NOTE: This doesn't check the transition function nor the found elements
-- nor the alternatives list.
unsafeDFS :: Eq v => (v -> [v]) -> [v] -> [v] -> [v]
unsafeDFS transition found alternatives = explore found alternatives
        where explore found []            = found
              explore found (alt : nexts) =
                if alt `elem` found
                    then explore (found)
                                 (nexts)
                    else explore (alt : found)
                                 (transition alt <> nexts)

depthFirstSearch :: Eq v => Digrafo v -> v -> [v]
depthFirstSearch graph = unsafeDFSPosOrder (trans graph) []
--depthFirstSearch graph = unsafeDFS (trans graph) [] . return

topologicalSort :: Eq v => Digrafo v -> [v]
topologicalSort graph = foldl' mix [] . vertices $ graph
        where mix sorted x = if x `elem` sorted
                                then sorted
                                else unsafeDFSPosOrder (trans graph) sorted x
---- good dfs, there's no repeated elements, but wrong global order
----topologicalSort graph = foldl' mix [] . vertices $ graph
----        where mix sorted x = if x `elem` sorted
----                                then sorted
----                                else unsafeDFS (trans graph) sorted [x]

--type Reverse a = [a]
--type Found   a = [a]
--
--unsafeDFSPre :: (Show v, Eq v) => (v -> [v]) -> [v] -> [v] -> Reverse v
--unsafeDFSPre transition found alternatives = explore found alternatives
--        where explore found []            = []
--              explore found (alt : nexts) =
--                if alt `elem` found
--                    then explore (found)
--                                 (nexts)
--                    else alt : explore (alt : found)
--                                       (transition alt <> nexts)

----topologicalSort2 :: (Show v, Eq v) => Digrafo v -> [v]
----topologicalSort2 graph = foldl' mix [] . vertices $ graph
----        where mix sorted x = if x `elem` sorted
----                                then sorted
----                                else unsafeDFSPos (trans graph) sorted [x] <> sorted

----newtype Queue a = Queue ([a],[a]) deriving (Show,Eq,Ord)

---- Queue (normal list,reversed list)
----singleton x = Queue ([x],[])
----
----emptyq = Queue ([],[])
----put x (Queue (nor,rev)) = Queue (nor, x : rev)
----toList (Queue (nor,rev)) = nor ++ reverse rev
----appendq first second = Queue ((toList first) ++ (toList second),[])

----unsafeDFSPosFail :: (Show v, Eq v) => (v -> [v]) -> [v] -> [v] -> Reverse v
----unsafeDFSPosFail transition found alternatives = toList $ explore found alternatives
----        where explore found []            = emptyq
----              explore found (alt : nexts) =
----                if trace ("alt: " ++ show alt ++ ", callstack: " ++ show found ++ ", nexts:" ++ show (transition alt <> nexts) ++ "\n") $ alt `elem` found
----                    then explore (found)
----                                 (nexts)
----                    else put alt $ explore (alt : found)
----                                           (transition alt <> nexts)

unsafeDFSPreReverse transition found alternatives = explore found alternatives
        where explore found []            = found
              explore found (alt : nexts) =
                if alt `elem` found
                    then explore (found)
                                 (nexts)
                    else explore (alt : found)
                                       (transition alt <> nexts)

-- | This is kinda good, I just have to reverse it
unsafeDFSPosOrder transition found x = explore found x
    where explore found alt =
                if alt `elem` found
                    then found
                    else alt : foldl' (\found x -> explore found x)
                                    found
                                    (transition alt)

-- This is like a Java's function name
unsafeDFSPosOrderReversed transition found = reverse . unsafeDFSPosOrder transition found



--        where explore found []            = []
--              explore found (alt : nexts) =
--                if alt `elem` found
--                    then explore (found)
--                                 (nexts)
--                    else explore (alt : found)
--                                 (transition alt <> nexts)

        -- early process
--    where explore found alternatives =
--            -- for each izq >> der
--            fold  (\(found,stack) x -> 
--                    if x `elem` found
--                        -- ignore search
--                        then (found,stack)
--                        else explore (x:found) (transition x)
--                    )
--                   (found,[])
--                   alternatives
-- Reversed PosOrder
--  where explore found alternatives =
--          -- for each izq >> der
--          foldl' (\found x -> 
--                  if x `elem` found
--                      -- ignore search
--                      then found
--                      else explore (x:found) (transition x)
--                  )
--                 found
--                 alternatives
--    explore [] found alternatives
--    where explore stack found [] = stack
--          explore stack found (alt : alternatives) =
--            if alt `elem` found
--                then explore stack found alternatives -- ignore alt
--                else alt : explore stack 

--unsafeDFSPos transition found alternatives = toList $ explore emptyq found alternatives
--        where explore queue found []            = queue
--              explore queue found (alt : searchLater) =
--                let nexts = transition alt
--                in if inspect found alt searchLater $ alt `elem` found
--                    -- just ignore the element
--                    then explore (queue)
--                                 (found)
--                                 (searchLater)
--                    else if null nexts
--                            -- it's a leaf, thus we have to put the element inmediately on the queue
--                            then explore (put alt queue)
--                                         (alt : found)
--                                         (transition alt <> searchLater)
--
--                            -- gotta wait a more time till the chilren are found
--                            else put alt $ explore (queue)
--                                                   (alt : found)
--                                                   (transition alt <> searchLater)
--              -- TODO: DELETE LATER
--              inspect found alt nexts = trace ("alt: " ++ show alt ++ ", callstack: " ++ show found ++ ", nexts:" ++ show (transition alt <> nexts) ++ "\n") . id

--                let nexts = transition alt
--                in if inspect found alt searchLater $ alt `elem` found
--                    -- just ignore the element
--                    then explore (queue)
--                                 (found)
--                                 (nexts)
--                    else if null nexts
--                            -- it's a leaf, thus we have to put the element inmediately on the queue
--                            then explore (put alt queue)
--                                         (alt : found)
--                                         (transition alt <> searchLater)
--
--                            -- gotta wait a more time till the chilren are found
--                            else put alt $ explore (queue)
--                                                   (alt : found)
--                                                   (transition alt <> searchLater)
--              -- TODO: DELETE LATER
--              inspect found alt nexts = trace ("alt: " ++ show alt ++ ", callstack: " ++ show found ++ ", nexts:" ++ show (transition alt <> nexts) ++ "\n") . id



--unsafeDFSRevPostOrder :: Eq v => (v -> [v]) -> [v] -> [v] -> Reverse v
--unsafeDFSRevPostOrder transition found alternatives = explore [] found alternatives
--        where explore revpost found []            = []
--              explore revpost found (alt : nexts) =
--                if alt `elem` found
--                    then explore (found)
--                                 (nexts)
--                    else alt : explore (alt : found)
--                                       (transition alt <> nexts)

-- too many repetitions
--topologicalSort graph = foldl' apply [] . vertices $ graph
--    where apply sorted x = if trace (show x ++ ": " ++ ", sorted: " ++ show sorted ++ ", " ++ show (x `elem` sorted)) $ x `elem` sorted
--                            then trace ("Ignore x") $ sorted
--                            else trace ("Update search") $ depthFirstSearch graph x <> sorted


data Tree a = Branch a (Tree a) (Tree a) | Leaf a deriving (Show,Eq,Ord)
preorder fun (Branch a l r) = fun a : (preorder fun l ++ preorder fun r)
preorder fun (Leaf   a    ) = [fun a]
tree1 = Branch 0 (Leaf 1) (Branch 2 (Leaf 3) (Leaf 4))

posorder fun (Branch a l r) = (posorder fun l ++ posorder fun r) ++ [fun a]
posorder fun (Leaf   a    ) = [fun a]


