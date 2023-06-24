-- Correction of HW1
--

import Data.List

-------------------------------
-- Exercise 1 -- Rebracketing
-------------------------------

-- Exercise 1.a
first = fst . fst
second = snd . fst
third = snd

triple = (("hello",8),"world")
colortriple = (("blue","green"),"red")

{-
ghci> first triple
"hello"
ghci> second triple
8
ghci> third triple
"world"
-}

-- Exercise 1.b

rebracket input = (first input,(second input,third input))

{-
ghci> rebracket triple
("hello",(8,"world"))
ghci> rebracket colortriple
("blue",("green","red"))
-}

-- Exercise 1.b (variant with pattern matching)
rebracketV2 ((x,y),z) = (x,(y,z))

--------------------------------
-- Exercise 2 -- Church numerals
--------------------------------

-- Exercise 2.a
churchZero = \f x -> x
churchOne = \f x -> f x
churchTwo = \f x -> f (f x)
churchThree = \f x -> f (f (f x))
churchFour = \f x -> f (f (f (f x)))

-- extra: coding integers into Church numerals
churchCode 0 = \f x -> x
churchCode n = \f x -> f (churchCode (n-1) f x)

-- another possible implementation of the churchCode
-- churchCode n f x = f (churchCode (n-1) f x)

-- Exercise 2.b
churchDecode num = num increment 0
     where increment = (+) 1

{-
ghci> churchDecode churchThree
3
-}

-- Exercise 2.c
churchPrint num = num successor "zero"
     where successor = (++) "succ "

{-
ghci> churchPrint churchThree
"succ succ succ zero"
-}

-- Exercise 2.d
churchOddEven num = num tilt 0
     where tilt = (-) 1

{-
ghci> churchOddEven churchTwo
0
ghci> churchOddEven churchThree
1
-}

-- Exercise 2.e
churchInc num = \f x -> num f (f x)
-- another possible implementation of the churchInc function
-- churchInc num = \f x -> f (num f x)

-- Exercise 2.f
churchAdd num1 num2 = \f x -> num1 f (num2 f x)

-- Exercise 2.g
churchMult num1 num2 = \f x -> num1 (num2 f) x
churchMultV2 num1 num2 = \f -> num1 (num2 f)

-- Exercise 2.h
churchPower num1 num2 = num2 num1

-- testing the power function with examples 
mysteryChurchNumber = churchPower (churchCode 2) (churchCode 10)
mysteryChurchNumber' = churchPower (churchCode 2) (churchCode 20)
mysteryChurchNumber'' = churchPower (churchCode 2) (churchCode 23)
mysteryChurchNumber''' = churchPower (churchCode 2) (churchCode 24)

{-
ghci> :set +sghci> churchDecode mysteryChurchNumber 
1024
(0.01 secs, 801,104 bytes)
ghci> churchDecode mysteryChurchNumber'
1048576
(1.63 secs, 766,461,056 bytes)
ghci> churchDecode mysteryChurchNumber''
8388608
(12.18 secs, 6,131,173,088 bytes)
ghci> churchDecode mysteryChurchNumber'''
*** Exception: stack overflow
-}

