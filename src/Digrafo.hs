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

-- | TODO: Delete later
grafo1 = (G [1..4] suc)
    where suc 1 = [2,3]
          suc 2 = [4]
          suc 3 = [4]
          suc 4 = []

data Digrafo v = G [v] (v -> [v])

vertices :: Digrafo v -> [v]
vertices (G vs _) = vs

duplicate a = (a,a)

-- utility function
trans (G _ t) = t


-- utility function
runTransition :: (a -> b) -> [a] -> [(a, b)]
runTransition transition = fmap (fmap transition . duplicate)



arcos :: Digrafo v -> [(v, v)]
arcos g = mergePairs arcsByRoot
    where arcsByRoot = runTransition (trans g) (vertices g)
          mergePairs = concat . fmap (\(root,nexts) -> fmap ((,) root) nexts)
          
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
nVertices = undefined

nArcos :: Digrafo v -> Int
nArcos = undefined

sucesores :: Eq v => Digrafo v -> v -> [v]
sucesores = undefined

antecesores :: Eq v => Digrafo v -> v -> [v]
antecesores = undefined

gradoSal :: Eq v => Digrafo v -> v -> Int
gradoSal = undefined

gradoEnt :: Eq v => Digrafo v -> v -> Int
gradoEnt = undefined

depthFirstSearch :: Eq v => Digrafo v -> v -> [v]
depthFirstSearch = undefined

topologicalSort :: Eq v => Digrafo v -> [v]
topologicalSort = undefined




