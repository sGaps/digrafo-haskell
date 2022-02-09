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

import Data.List (find, sort, groupBy)
import Data.Function (on)
import Data.Maybe (maybeToList)

-----------------------
-- | TODO: Delete later
grafo1 = (G [1..4] suc)
    where suc 1 = [2,3]
          suc 2 = [4]
          suc 3 = [4]
          suc 4 = []
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


-- TODO: Change `case` by `maybeToList`
-- total function
sucesores :: Eq v => Digrafo v -> v -> [v]
sucesores g v = fmap snd . filter ((== v) . fst) . arcos $ g

--sucesores g v = case search of
--                    Nothing    -> []
--                    Just nexts -> nexts
--            where search = fmap snd
--                            . find ((== v) . fst)
--                            . runTransition (trans g)
--                            . vertices $ g

swap (a,b) = (b,a) -- but also: uncurry (flip (,))

-- TODO: Redefine but by using runTransition instead of this
antecesores :: Eq v => Digrafo v -> v -> [v]
antecesores g v = fmap fst . filter ((== v) . snd) . arcos $ g

--antecesores = runTransition

-- THIS REQUIRES (Ord v, Eq v) => ...
--antecesores g v = case search of
--                    Nothing    -> []
--                    Just prevs -> prevs
--                where search = fmap snd
--                                . find ((== v) . fst)
--                                -- inverse of runTransition
--                                . fmap ((\(vs,nexts) -> (head vs, nexts)) . unzip)
--                                . groupBy ((==) `on` fst)
--                                . sort
--                                . fmap swap
--                                . arcos $ g


--antecesores g v = groupBy ((==) `on` snd) . arcos $ g
--antecesores g v = groupBy ((==) `on` fst) . sort . fmap swap . arcos $ g
        -- gather edges
        -- swap first and second slot of each element
        -- sort them
        -- group by their next vertex


gradoSal :: Eq v => Digrafo v -> v -> Int
gradoSal g = length . sucesores g

gradoEnt :: Eq v => Digrafo v -> v -> Int
gradoEnt g = length . antecesores g

depthFirstSearch :: Eq v => Digrafo v -> v -> [v]
depthFirstSearch = undefined

topologicalSort :: Eq v => Digrafo v -> [v]
topologicalSort = undefined




