quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

main = do
    let list = [5, 4, 7, 3, 8, 6, 9, 2, 1, 10]
    let sorted_list = quicksort list
    putStrLn (show sorted_list)
