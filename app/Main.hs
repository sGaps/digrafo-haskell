module Main where

import Digrafo

-- input
grafo1 = (G [1..4] suc)
    where suc 1 = [2,3]
          suc 2 = [4]
          suc 3 = [4]
          suc 4 = []

main :: IO ()
main = putStrLn "Tarea 03"
