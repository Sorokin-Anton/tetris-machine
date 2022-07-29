module GameOfLife where

import TetrisMachine
import qualified Data.Map as M
import Data.Map ((!?))
import Graphics.Gloss
import Data.Maybe (catMaybes)

main :: IO ()
main = playTetris defaultTetrisConfig{boardSize = (40,40)} handleEvents handleTime (\(a,_,_,_) -> a) initialModel

type Model = (TetrisPicture, Color, Float, Bool)
initialModel :: Model
initialModel= (M.empty, black, 0.5,True)

handleEvents :: TetrisConfig -> TetrisEvent -> Model -> Model
handleEvents _ (CellClicked LeftButton (m,n)) (pic, c, t,pz) = (M.insert (m,n) c pic, c, t,pz)
handleEvents _ (CellClicked RightButton (m,n)) (pic, c, t, pz) = (M.delete (m,n) pic, c, t, pz)
handleEvents _ (KeyPressed(Char 'r')) _ = initialModel
handleEvents _ (KeyPressed(Char '1')) (pic,_, t, pz) = (pic, green, t, pz)
handleEvents _ (KeyPressed(Char '2')) (pic,_, t, pz) = (pic, blue, t, pz)
handleEvents _ (KeyPressed(Char '3')) (pic,_, t, pz) = (pic, red, t, pz)
handleEvents _ (KeyPressed(Char 'p')) (pic,c, t, pz) = (pic, c, t, not pz)
handleEvents tc (KeyPressed(Char  's')) m = step  tc m
handleEvents _ _ pic = pic

handleTime :: TetrisConfig -> Float -> Model -> Model
handleTime tc t m@(pic,c, timeRest, pz) = if timeRest < t && not pz
  then case step tc m
    of (p,q,_,_) -> (p,  q, 0.5 + timeRest - t,pz)
  else (pic,c,timeRest - t, pz)

step :: TetrisConfig -> Model -> Model
step   TetrisConfig{boardSize = (sizeX, sizeY)} (cells, c, t, pz) = (M.fromList (catMaybes [
  case cells !? (i,j) of
    Just cellColor -> if length (catMaybes (map (cells !?) (neighbours (i,j)))) `elem` [2,3]
        then Just ((i,j), cellColor)
        else Nothing
    Nothing -> if length (catMaybes (map (cells !?) (neighbours (i,j)))) == 3
      then Just ((i,j), c)
      else Nothing
     | i <- [0 .. sizeX - 1], j <- [0 .. sizeY - 1]
  ]), c, t, pz)

neighbours :: (Coord, Coord) -> [(Coord, Coord)]
neighbours (m,n) = [(m+x, n+y) | x <- [-1,0,1], y <- [-1,0,1], not (x == 0 && y  == 0)]

