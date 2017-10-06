-- Quicksort 
module Qsort where
  qsort :: [Int] -> [Int]
  qsort [] = []
  qsort  (x:xs) =  qsort smaller ++ [x] ++ qsort larger
    where -- local definitions
      smaller = [n | n <- xs, n <= x] -- list comprehension + guards --> n alle elementen die kleiner zijn dan x
      larger = [n | n <- xs,  n > x]

  reverseqsort :: [Int] -> [Int]
  reverseqsort [] = []
  reverseqsort (x:xs) = reverseqsort larger ++ [x] ++ reverseqsort smaller
     where -- local definitions
      smaller = [n | n <- xs, n <= x] -- list comprehension + guards --> n alle elementen die kleiner zijn dan x
      larger = [n | n <- xs,  n > x]

