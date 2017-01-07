module CharBuffer (Buffer(..), Character(..), emptyBuffer, emptyStyle, characters, fromString, sets) where

import qualified Data.Set as S
import Data.List

type StyleName = String
data Style = Style (S.Set StyleName) deriving (Show, Eq)
data Character = Character{style :: Style, char :: Char} deriving (Show, Eq)


data Buffer = Buffer [[Character]]

emptyStyle = Style S.empty

emptyBuffer :: Int -> Int -> Buffer
emptyBuffer w h = Buffer $ replicate h $ replicate w $ Character (Style S.empty) ' '

fromString :: Style -> String -> [Character]
fromString style str = map (\x -> Character style x) str


characters :: [Character] -> String
characters cs = map (\c -> char c) cs

set :: Buffer -> Int -> Int -> Character -> Buffer
set (Buffer css) r c char = Buffer (h ++ (l' : t)) where 
  (h, (l : t)) = splitAt r css
  (pre, (_:post)) = splitAt c l
  l' = pre ++ (char:post)

sets :: Buffer -> Int -> Int -> [Character] -> Buffer
sets (Buffer css) r c str = Buffer (h ++ (l' : t)) where 
  (h, (l : t)) = splitAt r css
  cols = length l
  (pre, post') = splitAt c l
  post = drop (length str) post'
  l' = take cols $ pre ++ (str ++ post)

