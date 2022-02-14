{-|
Module      : Digrafo
Description : A simple module for modelling Directed Graphs.
Copyright   : (c) Gabriel Peraza, 2022
License     : MIT
Stability   : experimental

A simple Directed Graph made for a Programming Languages course.
-}

module Digrafo (
    -- * Data type
    Digrafo (G),
    -- * Operators
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

import Data.List (foldl')

-- | A Directed Graph data type. It holds a list with no repeated vertices and
--   a transition function that returns the next vertices which are in the
--   given list.
data Digrafo v = G [v] (v -> [v])

-- | Returns the Graph's vertices
vertices :: Digrafo v -> [v]
vertices (G vs _) = vs

-- utility function
-- | Returns the GRaph's transition/succesor function
trans (G _ t) = t


-- Function(Internals)

-- | Apply the given transition function on each elemenet of a list.
runTransition :: (a -> b) -> [a] -> [(a, b)]
runTransition transition = fmap (fmap transition . duplicate)
    where duplicate a = (a,a)


-- how does 'arcos' work?
--  runs the transition function over the list.
--  and then, it just merge the list of /options/ that was generated in the previous step.

-- | Returns the edge set the given Graph.
arcos :: Digrafo v -> [(v, v)]
arcos g = mergePairs arcsByRoot
    where arcsByRoot = runTransition (trans g) (vertices g)
          mergePairs = concat . fmap (\(root,nexts) -> fmap ((,) root) nexts)


-- | Returns the number of vertices in the graph. See also 'vertices'.
nVertices :: Digrafo v -> Int
nVertices = length . vertices

-- | Returns the number of edges in the graph. See also 'arcos'.
nArcos :: Digrafo v -> Int
nArcos = length . arcos


-- | Returns a list of the successors of the given vertex in a graph.
sucesores :: Eq v => Digrafo v -> v -> [v]
sucesores = trans

-- | Returns a list of the predecessors of the given vertex in a graph.
antecesores :: Eq v => Digrafo v -> v -> [v]
antecesores g v = fmap fst . filter ((== v) . snd) . arcos $ g

-- | Returns the number of successors of the given vertex in a graph.
--
-- See also: 'sucesores'.
gradoSal :: Eq v => Digrafo v -> v -> Int
gradoSal g = length . sucesores g

-- | Returns the number of predecessors of the given vertex in a graph.
--
-- See also: 'antecesores'.
gradoEnt :: Eq v => Digrafo v -> v -> Int
gradoEnt g = length . antecesores g


-- | Returns the Depth First Search of a given node in the graph.
depthFirstSearch :: Eq v => Digrafo v -> v -> [v]
depthFirstSearch graph = unsafeDFSPosOrder (trans graph) []

-- | Returns the vertices of a Directed Graph sorted by topological order.
--
-- This functions works correctly when this property is met:
--
-- prop> (nub . vertices) G == vertices G
--
-- See also 'depthFirstSearch'.
topologicalSort :: Eq v => Digrafo v -> [v]
topologicalSort graph = foldl' mix [] . vertices $ graph
        where mix sorted x = if x `elem` sorted
                                then sorted
                                else unsafeDFSPosOrder (trans graph) sorted x

-- | Performs a DFS and returns the visited vertices on post order.
unsafeDFSPosOrder :: (Eq v, Foldable f) => (v -> f v) -> [v] -> v -> [v]
unsafeDFSPosOrder transition found x = explore found x
    where explore found alt =
                if alt `elem` found
                    then found
                    else alt : foldl' (\found x -> explore found x)
                                    found
                                    (transition alt)

