import System.IO
import System.Environment

-- the getCounts function computes the number 
-- characters, words and lines in a String
getCounts :: String -> (Int,Int,Int)
getCounts input = (charCount, wordCount, lineCount)
 where charCount = length input
       wordCount = (length . words) input
       lineCount = (length . lines) input

-- the countsText function produces a String
-- describing the number of characters, words and lines.
-- Here, the unwords function creates a string from a list of strings
-- by inserting space characters between original strings
countsText :: (Int,Int,Int) -> String
countsText (cc,wc,lc) = unwords ["chars: "
                                  , show cc
                                  , " words: "
                                  , show wc
                                  , " lines: "
                                  ,  show lc]

{-
-- this version of the code does not work
-- because of the lazy evaluation mechanism
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
-- the problem comes from the fact that the file is closed
-- by the hClose instruction before the input has been evaluated!
  hClose file
-- indeed, when the summary function is defined, we are using input
-- but we do not need to evaluate it
  let summary = (countsText . getCounts) input
-- finally, at this point, summary is evaluated and because of this input as well.
-- but now the file is closed and the OS will not let us read from it anymore!
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
  putStrLn summary
-- this is the error message we get here:
-- > ./Haskell-Code-Lesson-24-lazy illustration.txt 
-- Haskell-Code-Lesson-24-lazy: illustration.txt: 
-- hGetContents: illegal operation (delayed read on closed handle)
-}

-- this version works because we force the evaluation of summary 
-- by permuting the putStrLn
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  putStrLn summary
  hClose file
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
 
{-
-- this version also works because we close the file using hClose
-- after (and not before!) the evaluation of summary has been performed
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
  hClose file
  putStrLn "well done!"
-}