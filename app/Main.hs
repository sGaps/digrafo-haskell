module Main where

import Digrafo

-- a given input
grafo1 = (G [1..4] suc)
    where suc 1 = [2,3]
          suc 2 = [4]
          suc 3 = [4]
          suc 4 = []

-- example from Algorithms 4th Ed. Sedgewick (page 576)
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

emptyline = putStrLn ""

demo :: String -> Digrafo Int -> IO ()
demo title graph = do
    putStrLn title
    
    putStrLn $ "Vertices:   " ++ (show . vertices  $ graph)
    putStrLn $ "# Vertifes: " ++ (show . nVertices $ graph)

    putStrLn $ "Arcos:      " ++ (show . arcos     $ graph)
    putStrLn $ "# Arcos:    " ++ (show . nArcos    $ graph)
    emptyline

    putStrLn $ "Lista de Sucesores:"
    traverse showSucc (vertices graph)
    emptyline

    putStrLn $ "Lista de Antecesores:"
    traverse showPrev (vertices graph)
    emptyline

    putStrLn $ "Lista de Grados de Salida:"
    traverse showGSal (vertices graph)
    emptyline

    putStrLn $ "Lista de Grados de Entrada:"
    traverse showGEnt (vertices graph)
    emptyline

    putStrLn $ "Depth First Search:"
    traverse showDFS (vertices graph)
    emptyline

    putStrLn $ "Topological Sort: " ++ show (topologicalSort graph)
    putStrLn "------------------------------------------------"
    emptyline

    where showSucc x = putStrLn $ "    valor: " ++ show x
                                                ++ ", sucesores:\t"
                                                ++ show (sucesores   graph x)
          showPrev x = putStrLn $ "    valor: " ++ show x
                                                ++ ", antecesores:\t"
                                                ++ show (antecesores graph x)
          showGSal x = putStrLn $ "    valor: " ++ show x
                                                ++ ", #Sal:\t"
                                                ++ show (gradoSal graph x)

          showGEnt x = putStrLn $ "    valor: " ++ show x
                                                ++ ", #Ent:\t"
                                                ++ show (gradoEnt graph x)

          showDFS  x = putStrLn $ "    valor: " ++ show x
                                                ++ ", dfs:\t"
                                                ++ show (depthFirstSearch graph x)

main :: IO ()
main = do
    putStrLn "Tarea 03"
    demo "Grafo 1" grafo1
    demo "Grafo 4" grafo4

