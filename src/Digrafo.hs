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

-- Strict fold-left operator
import Data.List (foldl')

data Digrafo v = G [v] (v -> [v])

vertices :: Digrafo v -> [v]
vertices (G vs _) = vs

-- utility function
-- | returns the transition/succesor function
trans (G _ t) = t


-- utility function
-- | Takes a transition function and runs it over each element of the given list.
--
-- returns 
runTransition :: (a -> b) -> [a] -> [(a, b)]
runTransition transition = fmap (fmap transition . duplicate)
    where duplicate a = (a,a)


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
sucesores = trans

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

topologicalSort :: Eq v => Digrafo v -> [v]
topologicalSort graph = foldl' mix [] . vertices $ graph
        where mix sorted x = if x `elem` sorted
                                then sorted
                                else unsafeDFSPosOrder (trans graph) sorted x

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

