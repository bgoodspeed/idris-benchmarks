

module Permutations



permutations : (Eq a) => List a -> List (List a)
permutations [] = [[]] 
permutations xs = [x::ys | x <- xs, ys <- permutations (delete x xs)] 

