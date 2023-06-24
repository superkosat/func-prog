import Data.List
--Exercise 1: Searching patterns inside DNA sequences
--1a: opposite strand
dualStrandSeq :: String -> String
dualStrandSeq [] = []
dualStrandSeq str
    | head str == 'A' = 'T' : dualStrandSeq (tail str)
    | head str == 'T' = 'A' : dualStrandSeq (tail str)
    | head str == 'G' = 'C' : dualStrandSeq (tail str)
    | head str == 'C' = 'G' : dualStrandSeq (tail str)

--1b: prefix of strand
--isPrefix :: String -> String -> Bool
isPrefix [] str2 = True
isPrefix str1 [] = False
isPrefix str1 str2 = if (head str1) == (head str2)
                        then isPrefix (tail str1) (tail str2)
                    else
                        False

--1c: substring of strand
isSubString :: String -> String -> Bool
isSubString [] str2 = True
isSubString str1 [] = False
isSubString str1 str2 = if (isPrefix str1 str2)
                            then True
                        else
                            isSubString str1 (tail str2)


--1d: subsequence of strand
isSubSeq [] str2 = True
isSubSeq str1 [] = False
isSubSeq str1 str2
    | (head str1) == (head str2) = isSubSeq (tail str1) (tail str2)
    | otherwise = isSubSeq str1 (tail str2)



--1e: list of substrings
listOfSubString :: String -> [String]
listOfSubString str = [substr i j | i <- [0..(length str)], j <- [0..((length str) - i)]]
 where substr i j = take j (drop i str)


--1f: list of subsequences
listOfSubSeq :: String -> [String]
listOfSubSeq "" = [""]
listOfSubSeq (x:xs) = listOfSubSeq xs ++ map (x:) (listOfSubSeq xs)


--1g: list of embeddings
--listOfEmbed str = []


--Exercise 2: Sieve of Eratosthenes
--2a:
eratoSieve :: Int -> [Int] -> [Int]
eratoSieve _ [] = []
eratoSieve m intList
    | (head intList) `mod` m == 0 = eratoSieve m (tail intList)
    | otherwise = (head intList):(eratoSieve m (tail intList))


--2b: List of prime numbers
listOfPrimes :: Int -> [Int]
listOfPrimes n = sieve [2..(n-1)] []
  where sieve [] primes = primes
        sieve (x:xs) primes = sieve (filter (\y -> y `mod` x /= 0) xs) (primes ++ [x])


--2c: listOfPrimesPowerOfTwoMinusOne
--listOfPrimesPowerOfTwoMinusOne :: Integer -> [Integer]
--Had trouble with solving this problem


--2d: Decomposition of prime numbers
--decompInPrimes :: Int -> [Int]
--Had trouble with solving this problem


--2e:



main = do
    print "Hello World!"
    let a = "ATCGCGCTA"
    let b = "ATCGC"
    print (dualStrandSeq a)
    print (isPrefix a b)
    print (isSubString a b)
    print (isSubString b a)
    print (eratoSieve 5 [2, 5, 2, 4, 7, 2, 10])
    print (listOfSubString "GATA")
    print (isSubSeq "ace" "abcde")
    print (listOfPrimes 100)
    print (listOfSubSeq "AGTC")