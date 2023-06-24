helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"


main :: IO ()
main = getLine >>= 
    (\name -> 
        (\statement -> 
            putStrLn statement) (helloPerson name))

    {-
    getLine >>= (putStrLn helloPerson)
    -}

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)