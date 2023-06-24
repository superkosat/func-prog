import Data.Function
import Data.List
import Control.Applicative
import Control.Monad

----------------------------------------------------
-- Exercice 1: Playing Chess using the List monad --
----------------------------------------------------

-- We start by defining the type ChessPos of chess positions

type ChessPos = (Int,Int)

-- The intuition is that every position of the board is
-- described by a pair (m,n) where m and n are between 1 and 8

------------------
-- Question 1.a -- 
------------------

-- Define a function 

onBoard :: ChessPos -> Bool

-- which tests whether a pair (p,q) is an element of the board
-- The function should behave like this:
-- ghci> onBoard (1,1)
-- True
-- ghci> onBoard (8,8)
-- True
-- ghci> onBoard (0,0)
-- False

------------------
-- Question 1.b -- 
------------------

-- Define a function

moveKnight :: ChessPos -> [ChessPos] 

-- which associates to every position (c,r) the list of positions 
-- on the board which the knight can access in one move.
-- The function should behave as follows:
--ghci> moveKnight (6,2)
--[(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]
--ghci> moveKnight (8,1)
--[(6,2),(7,3)]

------------------
-- Question 1.c -- 
------------------

-- This is the most interesting part of the exercise.
-- Use the do notation to define the function

in3 :: ChessPos -> [ChessPos]

-- which collects the positions which can be reached 
-- in three moves by the knight from a start position

-- The in3 function should behave in the following way:
--ghci> in3 (1,1)
--[(7,2),(3,2),(6,3),(4,3),(7,2),(7,4),(3,2),(3,4),(6,1),(6,5),(4,1),(4,5),(3,2),(2,3),(3,2),(3,4),(2,1),(2,5),(6,3),(6,5),(2,3),(2,5),(5,2),(5,6),(3,2),(3,6),(4,3),(4,5),(3,2),(3,6),(1,2),(1,6),(6,1),(6,3),(2,1),(2,3),(5,4),(3,4),(6,3),(6,5),(2,3),(2,5),(5,2),(5,6),(3,2),(3,6),(5,2),(1,2),(4,3),(2,3),(5,4),(5,6),(1,4),(1,6),(4,3),(4,7),(2,3),(2,7),(3,2),(2,3),(3,4),(3,6),(2,3),(2,7)]
-- Note that many positions are repeated in the list: the reason is that each occurrence of a position in the list
-- corresponds to a particular path to the position from the start position.

------------------
-- Question 1.d -- 
------------------

-- Reformulate the in3 function using a sequence of three binds

in3' :: ChessPos -> [ChessPos]

-- in the same way as we did in the class to translate
-- any do notation into a program with bind and then.

------------------
-- Question 1.e -- 
------------------

-- Define the canReachIn3 function

canReachIn3 :: ChessPos -> ChessPos -> Bool 

-- The canReachIn3 function behaves in the following way:
-- ghci> (6, 2) `canReachIn3` (6, 1) True
-- ghci> (6, 2) `canReachIn3` (7, 3) False

------------------------
-- Bonus question 1.f -- 
------------------------

-- Use the replicate function
-- replicate :: Int -> a -> [a]
-- and the foldl function to generalise 
-- the in3 function just defined into a function

inN :: Int -> ChessPos -> [ChessPos]

-- which computes the list of positions
-- reachable in exactly n steps for any integer n


----------------------------------------
-- Exercice 2: State monad and stacks --
----------------------------------------

-- We start by defining the type Stack of stacks
-- where a stack is understood as a list of integers

type Stack = [Int]

------------------
-- Question 2.a -- 
------------------

-- Give an elementary definition using pattern matchig
-- of the pop and push functions

popS :: Stack -> (Int, Stack) 

pushS :: Int -> Stack -> ((),Stack)

-- The definition can be checked using the following function

stackManipS :: Stack -> (Int, Stack) 
stackManipS stack = let
    ((), newStack1) = pushS 3 stack 
    (a , newStack2) = popS newStack1 
    in popS newStack2

-- which should behave in the following way:
-- ghci> stackManip [5,8,2,1]
-- (5,[8,2,1])

------------------
-- Question 2.b -- 
------------------

-- Now, we recall the State monad studied in the course. 
-- It is worth noting that the position of the types a and s 
-- have been switched in the definition of the State monad

newtype State s a = State ( s -> (a,s) )

-- The State monad comes together with the function

runState :: State s a -> s -> (a,s)
runState (State p) = p

-- Adapt the programs discussed in the course
-- and turn State into an instance of the Functor, 
-- the Applicative and the Monad type classes

instance Functor (State s) where
  fmap f (State h) = 

instance Applicative (State s) where
  pure x =
  (State k) <*> (State h) = 
                             
instance Monad (State s) where
  return x = 
  (State h) >>= f =

------------------
-- Question 2.c -- 
------------------

-- Define the popM and pushM functions of types

popM :: State Stack Int

pushM :: Int -> State Stack ()

-- which provide the monadic version of the functions
-- pop and push functions defined earlier as popS and pushS.
-- Check in particular that the function

stackManipM :: State Stack Int
stackManipM = do 
  pushM 3
  a <- popM
  popM

-- behaves in the same way as the stackManipS function
-- defined earlier, in the sense that:
-- ghci> runState stackManipM [5,8,2,1]
-- (5,[8,2,1])

------------------
-- Question 2.d -- 
------------------

-- Consider the function below 

stackStuffM :: State Stack () 
stackStuffM = do
   a <- popM 
   if a == 5
        then pushM 5
        else do
            pushM 3
            pushM 8

-- and reformulate it as a function

stackStuffB :: State Stack ()

-- using the bind and the then operators
-- in the same way as we did in the course.

-- We advise to check that the resulting code
-- works just as the original one:
-- ghci> runState stackStuffB [9,0,2,1,0]
-- ((),[8,3,0,2,1,0])
-- ghci> runState stackStuffM [9,0,2,1,0]
-- ((),[8,3,0,2,1,0])

------------------
-- Question 2.e -- 
------------------

-- Describe in a short comment the purpose 
-- of the get and put functions defined below.

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put newState = State $ \s -> ((), newState)

------------------
-- Question 2.f -- 
------------------

-- Define the pop and push functions using the do notation
-- as well as the get and put functions.

pop :: State Stack Int 

push :: Int -> State Stack () 

-- Hint: you can use the tail and head functions
-- in order to define the pop function

-- We advise to check that the resulting monadic code
-- works just as the previous one:

stackStuff :: State Stack () 
stackStuff = do
   a <- popM 
   if a == 5
        then push 5
        else do
            push 3
            push 8

-- and that one gets the expected behavior:
-- ghci> runState stackStuff [9,0,2,1,0]
-- ((),[8,3,0,2,1,0])

---------------------------------------------------------------
-- Exercice 3: An interpreter of a mini imperative language --
---------------------------------------------------------------

-- The purpose of the exercise is to implement an interpreter
-- of a small imperative language generalizing what we did in class
-- with arithmetic expressions. 
-- Note that we do not parse the code here, we simply interpret it.
--
-- We start by introducing the type Exp of expressions of our language
-- together with the type Stmt of statements (or programs)
--

type Var = String
type Val = Int

infixl 6 :+:, :-:
infixl 7 :*:, :/:

data Exp
  = C Int        -- constant with an integer value
  | V Var        -- variable with a name in Var = String                                                     
  | Exp :+: Exp  -- addition                                                     
  | Exp :-: Exp  -- subtraction                                                  
  | Exp :*: Exp  -- multiplication                                               
  | Exp :/: Exp  -- division

infix 1 :=

data Stmt
  = Var := Exp      -- assignment of the value of an expression to a variable                                          
  | While Exp Stmt  -- while loop as long as the boolean expression Exp holds                                                     
  | Seq [Stmt]      -- sequence of statements

-- A program is then simply defined as a statement of our language

type Prog = Stmt

-- Note that the infix are just here to help to write examples 
-- of programs in our mini-imperative language, 
-- such as the Fibonacci function below

fib :: Prog
fib = Seq
  [ "x" := C 0
  , "y" := C 1
  , While (V "n") $ Seq
      [ "z" := V "x" :+: V "y"
      , "x" := V "y"
      , "y" := V "z"
      , "n" := V "n" :-: C 1
      ]
  ]
  

------------------
-- Question 3.a -- 
------------------

-- We introduce the type Store of states of our imperative programs.
-- States are described here as lists of pairs consisting
-- of a variable in Var and of an integer value in Val.

type Store = [(Var, Val)]

-- Define the function

look :: Var -> Store -> Maybe Val

-- which takes a variable and a state as input
-- and returns the value of the variable in that state
-- when the value appears in the list of variables
-- and returns Nothing otherwise.
-- The function should behave as follows
-- ghci> look "x" [("n",5),("x",32),("y",67),("z",98)]
-- Just 32
-- ghci> look "m" [("n",5),("x",32),("y",67),("z",98)]
-- Nothing

------------------
-- Question 3.b -- 
------------------

-- Define the function

cleanup :: Store -> Store

-- which behaves in the following way:
-- ghci> cleanup [("n",5),("x",32),("x",67),("n",98)]
-- [("n",5),("x",32)]
-- The purpose of the function is to keep the first value 
-- assigned to a give variable in the list and to remove
-- the other (earlier) assignments of the variable.
-- The cleanup function enables us to think of a state
-- as a Map with strings as keys ad integers as values.

------------------
-- Question 3.c -- 
------------------

-- Complete the definition of the evaluation function

eval :: Exp -> Store -> Val
eval (C n) r       = ...
eval (V x) r       = case look x r of
                       Nothing -> error ("unbound variable `" ++ x ++ "'")
                       Just v  -> ...
eval (e1 :+: e2) r = ...
eval (e1 :-: e2) r = ...
eval (e1 :*: e2) r = ...
eval (e1 :/: e2) r = ... 

-- which computes the value of an expression in a given state.
-- The function should behave as follows:
-- ghci> eval (V "x" :+: V "y") [("n",5),("x",32),("y",67),("z",98)]
-- 99

------------------
-- Question 3.d -- 
------------------

-- Complete the definition of the exec function

exec :: Stmt -> Store -> Store
exec (x := e) r                    = ...
exec (While e s) r | eval e r /= 0 = exec (Seq [s, While e s]) r
                   | otherwise     = ...
exec (Seq []) r                    = ...
exec (Seq (s : ss)) r              = exec (Seq ss) (exec s r)

-- and check that it works as expected by testing it
-- with the fibonacci function 

run :: Prog -> Store -> Store
run p r = cleanup (exec p r)

-- run fib [("n", 25)]
-- [("n",0),("y",121393),("x",75025),("z",121393)]
-- ghci> look "x" $ run fib [("n", 25)]
-- Just 75025
