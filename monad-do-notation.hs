echoVerbose :: IO()
echoVerbose = putStrLn "Enter a Line: " >>
              getLine >>= putStrLn

main :: IO()
main = echoVerbose