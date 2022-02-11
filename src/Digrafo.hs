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

import Data.List (find, sort, groupBy, nub)
import Data.Function (on)
import Data.Maybe (maybeToList)
import Control.Monad(guard,forM)

import qualified Control.Monad.State as S

-----------------------
-- | TODO: Delete later
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


-- TODO: Refactor
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

-- TODO: Redefine but by using runTransition instead of this
antecesores :: Eq v => Digrafo v -> v -> [v]
antecesores g v = fmap fst . filter ((== v) . snd) . arcos $ g

gradoSal :: Eq v => Digrafo v -> v -> Int
gradoSal g = length . sucesores g

gradoEnt :: Eq v => Digrafo v -> v -> Int
gradoEnt g = length . antecesores g

depthFirstSearch :: Eq v => Digrafo v -> v -> [v]
depthFirstSearch graph vertex = explore [] [vertex]
    where explore found []            = found
          explore found (alt : nexts) =
            if alt `elem` found
                then explore (found)
                             (nexts)
                else explore (alt : found)
                             (sucesores graph alt <> nexts)
----- Only returns []
--depthFirstSearch graph vertex = explore [] [vertex]
--    where explore found []            = found
--          explore found (alt : nexts) =
--            if alt `elem` found
--                then []
--                else explore (alt : found)
--                             (sucesores graph alt <> nexts)
---- It doesn't go to right, only travels through left paths
--depthFirstSearch graph vertex = explore [] [vertex]
--    where explore _     []            = []
--          explore found (alt : nexts) =
--            if alt `elem` found
--                then []
--                else alt : explore (alt : found)
--                                   (sucesores graph alt <> nexts)

topologicalSort :: Eq v => Digrafo v -> [v]
topologicalSort = undefined

-- Deprecated
-- TODO: Remove
-- implicit vertex parameter
--  has duplicates
--dfs graph vertex = nub . explore [vertex] . nexts $ vertex
dfs graph vertex = explore [vertex] . nexts $ vertex
    where nexts   = sucesores graph
          explore _ [] = []
          explore found alternatives = do
                alt <- alternatives
                guard . not $ alt `elem` found

                let others = explore (alt:found) (nexts alt)

                alt:others

-- TODO: Remove
easyDFS graph vertex = explore [] [vertex]
    where explore _ [] = []
          explore found (alt:nexts) =
            if alt `elem` found
                then []
                else alt : explore (alt : found) ((sucesores graph alt) ++ nexts)


--type Found v = [v]
--type Alternative v = [v]

--statefulDFS :: Eq v => S.State (Found v) (Alternative v)
--statefulDFS []   = return []
--statefulDFS alts = forM 

--statefulDFS :: Eq v => Digrafo v -> v -> [v]
----statefulDFS graph vertex = runState . explore [vertex] . alternatives $ vertex
--statefulDFS graph vertex = runState . explore [vertex] $ []
--    where alternatives = sucesores graph
--
--          explore :: S.State (Found v) (Alternative v)
--          explore [] = return []
--          explore alts = do
--            forM 

