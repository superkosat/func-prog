{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

title :: T.Text
title = "望岳"   -- WÃ ng yuÃ¨

author :: T.Text
author = "æœç”«"   -- DÃ¹ FÇ”

verses :: [T.Text]
verses = ["岱 宗 夫 如 何"   -- dÃ i zÅng fÅ« rÃº hÃ©
        , "é½ é² é’ æœª äº†"   -- qÃ­ lÇ” qÄ«ng wÃ¨i liÇŽo
        , "é€  åŒ– é’Ÿ ç¥ž ç§€"   -- zÃ o huÃ  zhÅng shÃ©n xiÃ¹
        , "é˜´ é˜³ å‰² æ˜ æ™“"   -- yÄ«n yÃ¡ng gÄ“ hÅ«n xiÇŽo
        , "è¡ èƒ¸ ç”Ÿ å±‚ äº‘"   -- dÃ ng xiÅng shÄ“ng cÃ©ng yÃºn
        , "å†³ çœ¦ å…¥ å½’ é¸Ÿ"   -- juÃ© zÃ¬ rÃ¹ guÄ« niÇŽo
        , "ä¼š å½“ å‡Œ ç» é¡¶"   -- huÃ¬ dÄng lÃ­ng juÃ© dÇng
        , "ä¸€ è§ˆ ä¼— å±± å°"]  -- yÄ« lÇŽn zhÃ²ng shÄn xiÇŽo

shan :: T.Text
shan = "山"

shen :: T.Text
shen = "深"

verseToText :: [T.Text] -> T.Text
verseToText verses = mconcat (map breakline verses)
         where breakline verse = verse <> "\n"

poetry :: T.Text
poetry = verseToText verses

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where pieces = T.splitOn query fullText
        highlighted = mconcat ["[[", query, "]]"]

introText :: T.Text -> T.Text
introText pattern = mconcat ["Searching the symbol "
                           , pattern
                           , " in the poetry "
                           , title
                           , " by "
                           , author
                           , " :\n"]

main :: IO ()
main = do
    TIO.putStrLn (introText shan)
    TIO.putStrLn (highlight shan poetry)
    TIO.putStrLn (introText shen)
    TIO.putStrLn (highlight shen poetry)
