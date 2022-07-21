module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Model = (Float, Bool, [(Float, Float)])

main :: IO ()
main = play (InWindow "Nice Window" (800, 800) (10, 10)) white 60 initialWorld pic handleEvents handleTime

handleTime :: Float -> Model -> Model
handleTime t (x, b, arr) = (if b then x + t else x - t, b, arr)

handleEvents :: Event -> Model -> Model
handleEvents (EventKey (Char 'p') Down _ _) (x, b, arr) = (x, not b, arr)
handleEvents (EventKey (MouseButton LeftButton) Down _ (x1, y1)) (x, b, arr) = (x, b, (x1, y1) : arr)
handleEvents (EventKey (MouseButton RightButton) Down _ (x1, y1)) (x, b, arr) = (x, b, filter(\(x2, y2) -> sqrt ((x1 - x2) ^ 2 + (y1  -  y2) ^ 2) > 5)arr)
handleEvents _ m = m

initialWorld :: Model
initialWorld = (0, False,[])


pic :: Model -> Picture
pic (x, b, arr) = rotate (10 * x) (pictures [translate v 0 $ ThickCircle v 5 | v <- [40,60..200]]) <>
  pictures [translate x1 y1 $ color red $ circleSolid 5 | (x1, y1) <- arr]
