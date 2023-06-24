-- Basic parser with applicative and monadic combinators.
--
-- Based upon chapter 13 of Hutton's "Programming in Haskell" (2nd ed)
--
-- Eli Bendersky [http://eli.thegreenplace.net]
-- https://eli.thegreenplace.net/2017/deciphering-haskells-applicative-and-monadic-parsers/
-- This code is in the public domain.
--
-- Turned into its simplest possible form here
--
import Control.Applicative
import Data.Char

-- The idea is that a parser takes a string and then returns
-- the value Nothing when nothing has been parsed
-- the value Just of one pair of the element of type a
-- which has been parsed and the tail of the string

newtype Parser a = P { parse :: String -> Maybe (a,String) }

-- note that the Parser type constructor could be defined as
-- newtype Parser a = P ( String -> Maybe (a,String) )
-- while the parse function would be defined as follows:
-- parse :: Parser a -> String -> Maybe (a,String)
-- parse (P p) inp = p inp

-- let us turn Parser into an instance of the Functor type class

instance Functor Parser where

  -- fmap :: (a -> b) -> Parser a -> Parser b
  -- once the value v of type a has been obtained by the parser
  -- give that value v as argument to the function f :: a -> b
  -- and does not alter the remaining string

    fmap f p = P $ \inp -> case parse p inp of
                                    Nothing -> Nothing
                                    Just (v, out) -> Just(f v, out)


    
-- let us turn Parser into an instance of the Applicative type class

instance Applicative Parser where

  -- pure :: a -> Parser a
  -- the parser pure v returns the value v and does not touch at the string

  pure v = P (\inp -> Just(v,inp))

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  -- the parser pg <*> px applies the parser pg to the input string inp
  -- when the parser pg applied on inp produces Just (g,out) with g :: a -> b and out :: String
  -- then the parser pg <*> px applies the function g :: a -> b
  -- to the value of type a parsed by px on the tail string out.

  pg <*> px = P $ \inp -> case parse pg inp of
                                    Nothing -> Nothing
                                    Just (f, out) -> parse (fmap f px) out

-- now, let us turn Parser into an instance of the Monad type class

instance Monad Parser where

--  return :: a -> Parser a
  return = pure

  -- enables one to compose parsers:
  -- p >>= f parses the string using p then applies f 
  -- to the parsed value of type a in order to obtain 
  -- a parser of type Parser b which is applied
  -- to the tail of the string
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b

--  p >>= g =

-- The Alternative type class plays the same role on applicative functors
-- as the Monoid type class on usual types.
-- class Applicative f => Alternative f where
--    empty :: f a 
--    (<|>) :: f a -> f a -> f a
--    some :: f a -> f [a]
--    many :: f a -> f [a]
-- Minimal complete definition: empty, <|>
-- Here, some and many should be the least solutions of the equations:
--    some v = (:) <$> v <*> many v
--    many v = some v <|> pure []
-- In other words: 
-- some means one or more
-- many means zero or more

instance Alternative Parser where


  -- empty is the parser which takes the input and returns Nothing
  -- empty :: Parser a

  empty = P (\inp -> Nothing)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  -- the parser p <|> q makes p parse the input string
  -- and then returns the result (v,out) if the parsing works
  -- otherwise it makes the parser q parse the input string

  p <|> q = P (\inp -> case parse p inp of
                        Nothing -> Nothing
                        Just (v, out) -> parse (g v) out)

-- Basic building block: item parses a single char from the input.
-- Note that the type of the parser item is Parser Char because 
-- it returns a char together with the tail of the input string

item :: Parser Char
item = P (\inp -> case inp of
                []      -> Nothing
                (x:xs)  -> Just (x,xs))

-- sat takes a boolean test function p :: Char -> Bool 
-- and then returns the first character
-- (together with the tail of the input string)
-- when the first character is accepted by p
-- and the empty parser otherwise

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x 
                  else empty

-- digit is the parser which returns the first character
-- (together with the tail of the input string)
-- of an input string when the first character is a digit
-- Note that the function isDIgit :: Char -> Bool
-- is defined in the Data.Char module

digit :: Parser Char
digit = sat isDigit

-- here, we use the some function of the Alternative type class

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- char x is the parser which returns the first character
-- (together with the tail of the input string)
-- of the input string when this character is equal to x

char :: Char -> Parser Char
char x = sat (== x)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- BNF grammar for our language:
--
-- expr       ::= term '+' expr | term
-- term       ::= factor '*' term | factor
-- factor     ::= (expr) | int

expr :: Parser Int
expr = do x <- term
          char '+'
          y <- expr
          return (x+y)
       <|> term

term :: Parser Int
term = do x <- factor
          char '*'
          y <- term
          return (x*y)
       <|> factor

factor :: Parser Int
factor = do char '('
            x <- expr
            char ')'
            return (x)
         <|> int

-- testing the parser expr for expressions
-- using the parseExpr function

test :: String
test = "(1*2)+3+(4*5)"

test' :: String
test' = "helloworld"

test'' :: String
test'' = "(1*2)+3+(4*5)extrastuff"

parseExpr :: String -> String
parseExpr string = case parse expr string of
    Nothing -> "Sorry, I was not able to parse the expression " ++ string
    Just (val,[]) -> "The value of the expression " ++ string ++ " is " ++ show val
    Just (val,out) -> "The value of the expression " ++ string ++ " is probably " ++ show val ++ " although there is a tail " ++ out

output :: String
output = parseExpr test

output' :: String
output' = parseExpr test'

output'' :: String
output'' = parseExpr test''

factorE :: Parser Expr
factorE = do char '('
            x <- expr
            char ')'
            return (x)
         <|> int

intE :: Parser Expr
intE = do char "-"
            n <- nat
            return $ Val (-n)
        <|> natE

natE :: Parser Expr
natE = do xs <- some digit
            return $ Val $ read xs