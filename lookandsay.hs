--This module exports a function to generate a look-and-say sequence.
module LookAndSay(lookAndSay) where
    import Data.List


--Generates an infinite look 'n' say sequence.
    lookAndSay :: [Integer]
    lookAndSay = iterate (say) 1



{--Support functions--}
--Given an integer, "reads and says" to produce a number of the sequence. For example, 21 is composed by one number 2 and one number one,
--hence 1211.
    say :: Integer -> Integer
    say = synth . count . spell
    

--Given a number, recursively constructs the list of its digits.
    spell :: Integer -> [Integer]
    spell 0 = []
    spell n =
        spell (n `div` 10) ++ [n `mod` 10]


--Given a list of digits, converts it in the corresponding integer. 
--Note that the element with the lowest index will be the digit with the highest positional value: 
--each time an element is summed to the accumulator, this is multiplied by 10 to respect the decimal positional system.
    synth :: [Integer] -> Integer
    synth =
        let
            convert acc curr = 10*acc + curr
        in
            foldl' convert 0


--Given a list of integers (digits), creates a list in which each element is prependend with the number of times it appears consecutively.
--(similar to the so-called "run-length encoding", but flattened).
    count :: [Integer] -> [Integer]
    count =
        concatMap countOccurr . group
        where
            countOccurr n = [fromIntegral (length n), (head n)]


