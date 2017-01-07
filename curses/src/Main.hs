module Main where

import Control.Monad
import UI.NCurses
import qualified CharBuffer as CB
import qualified ConsoleRenderer as CR


buffer' = CB.sets (CB.emptyBuffer 10 10) 0 0 (CB.fromString CB.emptyStyle "Hello World what are you doing")
buffer = CB.sets buffer' 5 2 (CB.fromString CB.emptyStyle "Hello World what are you doing")
window = newWindow 12 12 5 5


main :: IO ()
main = do
	runCurses $ do 
		w <- window
		CR.render w buffer

		waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> unless (p ev') loop
