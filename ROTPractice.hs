data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where 
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize


largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
 where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L1,L2]

{-
fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
 where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
       rot4l = rotN alphaSize

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha,Alpha,Beta,Alpha,Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals =  map rot3l vals
 where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
       rot3l = rotN alphaSize


threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals =  map rot3ldecoder vals
 where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
       rot3ldecoder = rotNdecoder alphaSize

-}

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
 where halfN = n `div` 2
       offset = if even n
                then fromEnum c + halfN
                else 1 + fromEnum c + halfN
       rotation =  offset `mod` n

data ThreeLetterAlphabet = Alpha
                          | Beta
                          | Kappa deriving (Show,Enum,Bounded)

rotEncoder :: String -> String
rotEncoder text = map rotChar text
 where alphaSize = 1 + fromEnum (maxBound :: Char)
       rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text =  map rotCharDecoder text
 where alphaSize = 1 + fromEnum (maxBound :: Char)
       rotCharDecoder = rotNdecoder alphaSize

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals =  map rot3l vals
 where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
       rot3l = rotN alphaSize

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals =  map rot3ldecoder vals
 where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
       rot3ldecoder = rotNdecoder alphaSize

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
 where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
       rot4l = rotN alphaSize

fourLetterDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterDecoder vals =  map rot4ldecoder vals
 where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
       rot4ldecoder = rotNdecoder alphaSize





xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool,Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
 where remainder = n `mod` 2
       nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
   where reversedBits = reverse  (intToBits' n)
         missingBits = maxBits - (length reversedBits)
         leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
 where size = length bits
       indices = [size-1,size-2 .. 0]
       trueLocations = filter (\x -> fst x == True)
                       (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)




main = do
    print(rotN 4 L1)
    print(fourLetterAlphabetEncoder message)
    print(xorBool True True)
    print(xorBool True False)
    print(bitsToInt (intToBits' 21))