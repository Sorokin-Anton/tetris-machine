module GameOfLife where

import TetrisMachine
import qualified Data.Map as M
import Data.Map ((!?))
import Graphics.Gloss
import Data.Maybe (catMaybes)

main :: IO ()
main = playTetris modelConfig handleEvents handleTime modelPic initialModel

data Model =
  Model{modelPic  :: TetrisPicture
      , modelActiveColor :: Color
      , modelTimeTillTick ::  Float
      , modelIsPaused :: Bool
      , modelConfig :: TetrisConfig}
initialModel :: Model
initialModel= Model M.empty black 0.5 True defaultTetrisConfig{boardSize = (40,40)}

handleEvents :: TetrisEvent -> Model -> Model
handleEvents (CellClicked LeftButton (m,n)) model = model{modelPic = M.insert (m,n) (modelActiveColor model) (modelPic model)}
handleEvents (CellClicked RightButton (m,n)) model = model{modelPic = M.delete (m,n) (modelPic model)}
handleEvents (KeyPressed(Char 'r')) _ = initialModel
handleEvents (KeyPressed(Char '1')) model = model{modelActiveColor = black}
handleEvents (KeyPressed(Char '2')) model = model{modelActiveColor = blue}
handleEvents (KeyPressed(Char '3')) model = model{modelActiveColor = red}
handleEvents (KeyPressed(Char '4')) model = model{modelActiveColor = green}
handleEvents (KeyPressed(Char 'p')) model = model{modelIsPaused = not $ modelIsPaused model}
handleEvents (KeyPressed(Char  's')) model = step model
handleEvents _ model = model

handleTime :: Float -> Model -> Model
handleTime t model
  | modelIsPaused model = model
  | modelTimeTillTick model  < t = (step model){modelTimeTillTick =  0.5 + modelTimeTillTick model - t}
  | otherwise = model{modelTimeTillTick = modelTimeTillTick model - t}

step :: Model -> Model
step model = let TetrisConfig{boardSize = (sizeX, sizeY)} = modelConfig model in
  model{modelPic = M.fromList (catMaybes [
    case modelPic model !? (i,j) of
      Just cellColor -> if length (catMaybes (map (modelPic model !?) (neighbours (i,j)))) `elem` [2,3]
          then Just ((i,j), cellColor)
          else Nothing
      Nothing -> if length (catMaybes (map (modelPic model !?) (neighbours (i,j)))) == 3
        then Just ((i,j), modelActiveColor model)
        else Nothing
      | i <- [0 .. sizeX - 1], j <- [0 .. sizeY - 1]
  ])  }

neighbours :: (Coord, Coord) -> [(Coord, Coord)]
neighbours (m,n) = [(m+x, n+y) | x <- [-1,0,1], y <- [-1,0,1], not (x == 0 && y  == 0)]
