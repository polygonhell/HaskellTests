module ConsoleRenderer (ConsoleRenderer.render) where


import Control.Monad
import UI.NCurses as NC
import CharBuffer
import Data.List

render :: Window -> Buffer -> Curses ()
render w (Buffer css) = do
  setEcho False
  updateWindow w $ do
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 
    foldl (\a b -> a >> (renderLine (fst b) (snd b))) (return ()) cssWithRow
    moveCursor 3 4
  NC.render
  where
    cssWithRow = zip [1..toInteger (length css)] css
    renderLine r line = do
      moveCursor r 1
      drawString $ characters line
