--
-- Homework 3
-- Functors, Applicatives and Monads
-- Three case studies: Tree, Reader and State
--

--------------------------------------------
-- Exercise 1. The Tree type constructor  --
--------------------------------------------

-- the Tree type constructor is defined as we have seen in class:

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving Show



-- 1a. in order to test our code, we define a function

fromList :: [a] -> Tree a
fromList [] = error "list shouldn't be empty"
fromList [x] = Leaf x
fromList (x:xs) = Node (Leaf x) (fromList (x:xs))

treeAB :: Tree String
treeAB = fromList ["A","B"]

tree12 :: Tree String
tree12 = fromList ["1","2"]

-- which should have the following behaviour
-- ghci> treeAB
-- Node (Leaf "A") (Leaf "B")
-- ghci> fromList ["A","B","C"]
-- Node (Leaf "A") (Node (Leaf "B") (Leaf "C"))
-- ghci> fromList ["A","B","C","D"]
-- Node (Leaf "A") (Node (Leaf "B") (Node (Leaf "C") (Leaf "D")))

-- 1b. turn the Tree type constructor into an instance of the Functor type class

instance Functor Tree where
    fmap f (Leaf val) = Leaf (f val)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- 1b. similarly, turn Tree into an Applicative

instance Applicative Tree where
    pure val = Leaf val
    Leaf f <*> tree = f <$> tree
    Node l r <*> tree = Node (l <*> tree) (r <*> tree)

-- in such a way that when one defines

treeABfun :: Tree (String -> String)
treeABfun = fromList [(++) "A",(++) "B"]

-- one gets the following behaviour:
-- ghci> treeABfun <*> tree12
-- Node (Node (Leaf "A1") (Leaf "A2")) (Node (Leaf "B1") (Leaf "B2"))

-- 1c. similarly, turn Tree into a Monad

instance Monad Tree where
    return = pure
    (Leaf a) >>= f = f a
    (Node l r) >>= f = Node (l >>= f) (r >>= f)

-- in such a way that when one defines the tree

seqABthen12 :: Tree String
seqABthen12 = fmap mconcat $ sequence [treeAB,tree12]

-- one gets the following value
-- ghci> seqABthen12
-- Node (Node (Leaf "A1") (Leaf "A2")) (Node (Leaf "B1") (Leaf "B2"))



----------------------------------------------
-- Exercise 2. The Reader type constructor  --
----------------------------------------------

-- We define now the Reader type constructor as follows:

newtype Reader s a = Reader (s -> a)

-- The intuition is that s is a type of states.
-- A program of type Reader s a is thus a program of type a
-- which has the ability to read the value of the state of type s.



-- 2a. turn Reader into an instance of the Functor type class

instance Functor (Reader s) where
    fmap f (Reader sa) = Reader $ \s -> f (sa s)

-- 2b. turn Reader into an instance of the Applicative type class

instance Applicative (Reader s) where
    pure a = Reader $ \_ -> a
    (Reader sab) <*> (Reader sa) = Reader $ \s -> sab s (sa s)

-- 2c. turn Reader into an instance of the Monad type class

instance Monad (Reader s) where
    return a = Reader $ \_ -> a
    (Reader sa) >>= f = Reader $ \s -> let (Reader sb) = f (sa s) in sb s



--------------------------------------------
-- Exercise 3. The State type constructor --
--------------------------------------------

-- We define now the State type constructor as follows:

newtype State s a = State (s -> (s, a))

-- Just as in the case of Reader, the intuition is that s 
-- is a type whose values are the states available to the program.
-- A program of type State s a is thus a program of type a
-- which has the ability to read the value of the input state of type s
-- and then write the value of the output state of same type s.



-- 3a. turn State into an instance of the Functor type class

instance Functor (State s) where
    fmap f (State h) = State $ \s -> let (newState, x) = h s
                                            in (newState, f x)

-- 3b. turn State into an instance of the Applicative type class

instance Applicative (State s) where
  pure x = State $ \s -> (s, x)
  (State f) <*> (State g) = State $ \s -> let (s', h) = f s
                                              (s'', x) = g s'
                                          in (s'', h x)
    

-- 3c. turn State into an instance of the Monad type class

instance Monad (State s) where
  return = pure
  (State f) >>= g = State $ \s -> let (s', x) = f s
                                      (State h) = g x
                                  in h s'

-- 3d. our purpose below is to explore the State Int monad 
-- where Int defined the type of states available to the program.
-- We start by defining the action of type (State Int String)
-- which reads the value of the state n in Int, increments it to n+1
-- and shows n as a String 

getNext :: State Int String
getNext = State (\n -> (n+1, show n))

-- the list contains five times the element getNext

functions :: [State Int String]
functions = replicate 5 getNext

initialstate :: Int
initialstate = 0

-- Apply the function sequence provided by the Monad type class
-- sequence :: [State Int a] -> State Int [a]
-- to the list functions in order to define the list numbers
-- which should behave as follows:
-- ghci> numbers
-- ["0","1","2","3","4"]

numbers = let State f = sequence functions
                (outstate, numbers) = f initialState
                in numbers