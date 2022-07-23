module Main where
import TetrisMachine
import qualified Data.Map as M
import Graphics.Gloss (green, red)

main :: IO ()
main = playTetris defaultTetrisConfig handleEvents (const id) id M.empty

handleEvents :: TetrisEvent -> TetrisPicture -> TetrisPicture
handleEvents (CellClicked LeftButton (m,n)) pic = M.insert (m,n)  green pic
handleEvents (CellClicked RightButton (m,n)) pic = M.insert (m,n)  red pic
handleEvents (KeyPressed(Char 'r')) _ = M.empty
handleEvents _ pic = pic
