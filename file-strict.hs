{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI

-- the getCounts function computes the number 
-- characters, words and lines in a T.Text

getCounts :: T.Text -> (Int,Int,Int)
getCounts input = (charCount, wordCount, lineCount)
 where charCount = T.length input
       wordCount = (length . T.words) input
       lineCount = (length . T.lines) input

{-
the countsText function produces a T.Text
describing the number of characters, words and lines
The type signature of the T.pack function:
T.pack :: String -> T.Text
-}

countsText :: (Int,Int,Int) -> T.Text
countsText (cc,wc,lc) = T.pack (unwords ["chars: "
                                   , show cc
                                   , " words: "
                                   , show wc
                                   , " lines: "
                                   ,  show lc])

{-
the main function uses the do-notation
to get the name of the file as first argument
to assign the content of the file to input
to call the countsText and getCounts functions
to write these data in the file stats.dat
and finally to print these data on the screen

The type signature of the TI.appendFile function:
TI.appendFile :: FilePath -> T.Text -> IO ()
The type signature of the TI.putStrLn function:
TI.putStrLn :: T.Text -> IO ()
-}

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TI.readFile fileName
  let summary = (countsText . getCounts) input
  TI.appendFile "stats.dat"
                (mconcat [(T.pack fileName), " ",summary, "\n"])
  TI.putStrLn summary

{-
-- the code based on lazy evaluation does not work
main :: IO()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  hClose file
  let summary = (countsText . getCounts) input
  appendFile "stats.dat" (mconcat
  [fileName, " ", summary, "\n"])
  putStrLn summary
-}