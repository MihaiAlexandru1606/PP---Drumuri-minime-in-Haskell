module Tema1 (
        solveSimple,
        solveCosts
        ) where

import Data.Maybe
import Data.Array

inf = 10000005

-- functi pentru lucru pe tupluri
first (a, _, _) = a
second (_, b, _) = b
third (_, _, c) = c

findValue i j l = 
    let 
        find = (if i == j then [(i, j, 0)] else filter (\x -> (first x ) == i && (second x) == j ) l )
        rezult = (if find == [] then inf else (third (head find) ) )
    in rezult

init_graph n l = 
    let
        bounds = ((0, 0), (n, n))
        graph = listArray bounds [findValue i j l | (i, j) <- range bounds]
    in graph

floyd_warshall start end n graph =
    let
        bounds = ((0, 0, 0), (n, n, n))
        dp = listArray bounds [shortPath i j k | (i, j, k) <- range bounds]
        shortPath i j k
            | k == 0 = ( (graph ! (i, j) ), []) -- pereche distanata si path
            | otherwise = 
                    let currentPath = fst (dp ! (i, j, k - 1) )
                        newPath = (fst (dp ! (i, k, k - 1)) ) + (fst (dp ! (k, j, k - 1) ) )
                    in if currentPath > newPath 
                        then (newPath, (snd (dp ! (i, k, k - 1)) ) ++ [k] ++ (snd (dp ! (k, j, k - 1) ) ))
                        else (dp ! (i, j, k - 1) )

    in dp ! (start, end, n)

-- algoritmul Floyd-Warshall pentru task-ul 2
floyd_warshall_cost start end n graph sum cost =
    let
        bounds = ((0, 0, 0), (n, n, n))
        cost_entry = listArray (0, n) ([0] ++ cost)
        dp = listArray bounds [shortPath i j k | (i, j, k) <- range bounds]
        shortPath i j k
            | k == 0 = if (cost_entry ! j) > sum
                        then ( inf , [], inf ) -- nu se poate intra in orasul j plecand din orasul i
                        else ( (graph ! (i, j) ), [], (cost_entry ! j) ) 

            | otherwise =
                    let currentPath = first (dp ! (i, j, k - 1) )
                        newPath = (first (dp ! (i, k, k - 1)) ) + (first (dp ! (k, j, k - 1) ) )
                        currentCost =  ( third (dp ! (i, j, k - 1) ) )
                        newCost = ( (third (dp ! (i, k, k - 1)) ) ) + ( (third (dp ! (k, j, k - 1) ) ) )

                    in if (currentPath > newPath) && (sum >= newCost) 
                        then (newPath, (second (dp ! (i, k, k - 1)) ) ++ [k] ++ (second (dp ! (k, j, k - 1) ) ), newCost)

                        else
                            if (newCost < currentCost) && (sum >= newCost) && (currentPath == newPath)
                                then (newPath, (second (dp ! (i, k, k - 1)) ) ++ [k] ++ (second (dp ! (k, j, k - 1) ) ), newCost)
                                else (dp ! (i, j, k - 1) )
                        
    in dp ! (start, end, n)

create_list_cost [] _ _ = []
create_list_cost (node : listNode) cost sum = [(node, sum - (cost ! node)) ] ++ (create_list_cost listNode cost (sum - (cost ! node)))    

solveSimple :: (Int, [(Int, Int, Int)]) -> Maybe ([Int], Int)
solveSimple (n, listArc) = 
     let
        graph = init_graph n listArc
        rezult = floyd_warshall 1 n n graph
     in if (fst (rezult)) == inf 
        then Nothing
        else Just ([1] ++ (snd rezult )  ++ [n] , (fst (rezult)) )
           

solveCosts :: (Int, Int, [Int], [(Int, Int, Int)]) -> Maybe ([(Int, Int)], Int)
solveCosts (n, sum, cost, listArc) =
    let
        graph = init_graph n listArc
        rezult = floyd_warshall_cost 1 n n graph sum cost
    in if (first (rezult) ) == inf
        then Nothing
        else
            Just([(1, sum)] ++ ( create_list_cost ( (second rezult) ++ [n] ) (listArray (0, n) ([0] ++ cost) ) sum ), first rezult )


