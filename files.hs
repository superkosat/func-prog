import System.IO


hGetContents' :: Handle -> IO String
hGetContents' handle = do e <- hIsEOF handle
                            if e then return ""
                                   else do c <- hGetChar handle
                                        cs <- hGetContents' handle
                                        return (c:cs)





main :: IO ()
main = do
