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


data Digrafo v = G [v] (v -> [v])

vertices :: Digrafo v -> [v]
vertices = undefined

arcos :: Digrafo v -> [(v, v)]
arcos = undefined

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




