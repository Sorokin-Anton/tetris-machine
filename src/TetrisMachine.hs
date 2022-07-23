module TetrisMachine
(module TetrisMachine, module Graphics.Gloss.Interface.Pure.Game)
 where
import Graphics.Gloss.Interface.Pure.Game (Key (MouseButton), Event (EventKey), KeyState (Down), MouseButton(..), Key(..))
import Graphics.Gloss
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

type Coord = Int

data TetrisConfig = TetrisConfig {boardSize :: (Int, Int), cellSide :: Float}

defaultTetrisConfig :: TetrisConfig
defaultTetrisConfig =  TetrisConfig {boardSize = (16, 18), cellSide = 25}

type TetrisPicture = Map (Coord, Coord) Color

testPicture :: TetrisPicture
testPicture = M.fromList [((0,0), green), ((3,2), black), ((4,2), red),((5,2), red),((3,3), blue),((15,17), green)]

data TetrisEvent = CellClicked MouseButton (Coord, Coord) | KeyPressed Key

mapEvents :: TetrisConfig -> Event -> Maybe TetrisEvent
mapEvents TetrisConfig{boardSize = (sizeX, sizeY), cellSide = side} (EventKey (MouseButton mb)  Down _ (x,y)) =
  let
    m = floor $  (x + (fromIntegral sizeX * side / 2)) / side
    n = floor $  (y + (fromIntegral sizeY * side / 2)) / side
     in if m >= 0 &&  m < sizeX && n >= 0 &&  n < sizeY then Just $ CellClicked mb (m,n) else Nothing
mapEvents _ (EventKey key  Down _ _) = Just $ KeyPressed key
mapEvents _ _ = Nothing


playTetris :: TetrisConfig -> (TetrisEvent -> k -> k) -> (Float -> k -> k) -> (k -> TetrisPicture) -> k -> IO ()
playTetris tc handleEvents handleTime  draw start =
  play
  (InWindow "Tetris machine" (800, 800) (10, 10))
  white
  60
  start
  (drawTetris tc . draw)
  (maybe id handleEvents . mapEvents tc) handleTime

drawTetris :: TetrisConfig -> TetrisPicture -> Picture
drawTetris TetrisConfig{boardSize = (sizeX, sizeY), cellSide = side} pixelData =
  translate (- toPixels sizeX / 2)  (- toPixels sizeY / 2) $ pictures $
    catMaybes [translate (toPixels x) (toPixels y) <$> case pixelData M.!? (x,y) of
      Nothing -> Nothing
      Just c -> Just $ square c side
       | x <- [0 .. sizeX - 1], y  <- [0 .. sizeY - 1]]
    <> [line [(toPixels x, 0), (toPixels x, toPixels sizeY)] | x <- [0 .. sizeX]]
    <> [line [(0,toPixels y), (toPixels sizeX, toPixels y)] | y <- [0 .. sizeY]]
  where
    toPixels :: Int -> Float
    toPixels n = fromIntegral n * side

square :: Color -> Float -> Picture
square c d = color c $ polygon [(0,0),(d,0),(d,d),(0,d)]
