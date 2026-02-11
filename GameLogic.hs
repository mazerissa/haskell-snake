{-# LANGUAGE RecordWildCards #-}

module GameLogic
  ( Point
  , Direction(..)
  , Snake
  , GameState(..)
  , initialGameState
  , updateGame
  , isGameOver
  , getInputsForNN
  ) where

import System.Random (Random, randomR, mkStdGen)

type Point = (Float, Float)

data Direction = DUp | DDown | DLeft | DRight
  deriving (Show, Eq)

type Snake = [Point]

data GameState = GameState
  { snake :: Snake
  , food :: Point
  , direction :: Direction
  , score :: Int
  , isAlive :: Bool
  , timeAlive :: Int
  , gridSize :: Float
  , seed :: Int
  } deriving (Show)

initialGameState :: Int -> GameState
initialGameState s = GameState
  { snake = [(0, 0), (-20, 0), (-40, 0)]
  , food = (100, 100)
  , direction = DRight
  , score = 0
  , isAlive = True
  , timeAlive = 0
  , gridSize = 400
  , seed = s
  }

updateGame :: Direction -> GameState -> GameState
updateGame newDir state@GameState{..}
  | not isAlive = state
  | otherwise = 
      let newSnake = moveSnake newDir snake
          headPos = head newSnake
          ateFood = headPos == food
          finalSnake = if ateFood then newSnake else init newSnake
          newFood = if ateFood then generateFood state else food
          newScore = if ateFood then score + 10 else score
          collision = checkCollision headPos finalSnake gridSize
          newTimeAlive = timeAlive + 1
      in state
          { snake = finalSnake
          , food = newFood
          , direction = newDir
          , score = newScore
          , isAlive = not collision
          , timeAlive = newTimeAlive
          , seed = seed + 1
          }

moveSnake :: Direction -> Snake -> Snake
moveSnake dir (h:t) = newHead : h : t
  where
    (x, y) = h
    newHead = case dir of
      DUp    -> (x, y + 20)
      DDown  -> (x, y - 20)
      DLeft  -> (x - 20, y)
      DRight -> (x + 20, y)
moveSnake _ [] = []

checkCollision :: Point -> Snake -> Float -> Bool
checkCollision (x, y) snakeBody gridSize =
  wallCollision || selfCollision
  where
    wallCollision = abs x > gridSize / 2 || abs y > gridSize / 2
    selfCollision = (x, y) `elem` tail snakeBody

generateFood :: GameState -> Point
generateFood GameState{..} =
  let gen = mkStdGen seed
      (rx, gen') = randomR (-9, 9 :: Int) gen
      (ry, _) = randomR (-9, 9 :: Int) gen'
      fx = fromIntegral rx * 20
      fy = fromIntegral ry * 20
  in (fx, fy)

isGameOver :: GameState -> Bool
isGameOver = not . isAlive

getInputsForNN :: GameState -> [Double]
getInputsForNN GameState{..} =
  let (hx, hy) = head snake
      (fx, fy) = food
      halfGrid = gridSize / 2
      
      visionRays = map (castRay (hx, hy) snake food halfGrid False) directions8
      foodRays = map (castRay (hx, hy) snake food halfGrid True) directions8
      
      foodDirX = realToFrac (fx - hx) / gridSize
      foodDirY = realToFrac (fy - hy) / gridSize
      foodDist = sqrt (foodDirX * foodDirX + foodDirY * foodDirY)
      foodAngle = atan2 (realToFrac (fy - hy)) (realToFrac (fx - hx)) / pi
      
      dangerUp = if wouldCollideOrTrap (hx, hy + 20) DUp snake halfGrid then 1.0 else 0.0
      dangerDown = if wouldCollideOrTrap (hx, hy - 20) DDown snake halfGrid then 1.0 else 0.0
      dangerLeft = if wouldCollideOrTrap (hx - 20, hy) DLeft snake halfGrid then 1.0 else 0.0
      dangerRight = if wouldCollideOrTrap (hx + 20, hy) DRight snake halfGrid then 1.0 else 0.0
      
      (dirUp, dirDown, dirLeft, dirRight) = case direction of
        DUp -> (1.0, 0.0, 0.0, 0.0)
        DDown -> (0.0, 1.0, 0.0, 0.0)
        DLeft -> (0.0, 0.0, 1.0, 0.0)
        DRight -> (0.0, 0.0, 0.0, 1.0)
      
      tailPos = if length snake > 1 then snake !! (length snake - 1) else (hx, hy)
      (tx, ty) = tailPos
      tailDirX = realToFrac (tx - hx) / realToFrac gridSize
      tailDirY = realToFrac (ty - hy) / realToFrac gridSize
      
      manhattanDist = abs (fx - hx) + abs (fy - hy)
      efficiency = realToFrac manhattanDist / max 1.0 (realToFrac gridSize)
      
  in visionRays ++ foodRays ++
     [realToFrac foodDirX, realToFrac foodDirY, realToFrac foodDist, realToFrac foodAngle] ++
     [dangerUp, dangerDown, dangerLeft, dangerRight] ++
     [dirUp, dirDown, dirLeft, dirRight] ++
     [tailDirX, tailDirY, realToFrac (length snake) / 50.0, efficiency]

directions8 :: [(Float, Float)]
directions8 = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

castRay :: Point -> Snake -> Point -> Float -> Bool -> (Float, Float) -> Double
castRay (hx, hy) snake food halfGrid detectFood (dx, dy) =
  let normalized = normalizeVec (dx, dy)
      (ndx, ndy) = normalized
      maxDist = halfGrid * 2
      foodDist = distanceToPoint (hx, hy) food (ndx, ndy)
      wallDist = distanceToWall (hx, hy) halfGrid (ndx, ndy)
      bodyDist = distanceToBody (hx, hy) snake (ndx, ndy)
      minObstacle = minimum [wallDist, bodyDist]
  in if detectFood
     then if foodDist < 100 then realToFrac (1.0 - foodDist / 100.0) else 0.0
     else realToFrac (minObstacle / maxDist)

normalizeVec :: (Float, Float) -> (Float, Float)
normalizeVec (x, y) =
  let mag = sqrt (x * x + y * y)
  in if mag > 0 then (x / mag, y / mag) else (0, 0)

distanceToPoint :: Point -> Point -> (Float, Float) -> Float
distanceToPoint (x1, y1) (x2, y2) (dx, dy) =
  let vx = x2 - x1
      vy = y2 - y1
      dot = vx * dx + vy * dy
  in if dot > 0 then sqrt (vx * vx + vy * vy) else 999999

distanceToWall :: Point -> Float -> (Float, Float) -> Float
distanceToWall (x, y) halfGrid (dx, dy)
  | dx > 0 = (halfGrid - x) / dx
  | dx < 0 = (halfGrid + x) / (-dx)
  | dy > 0 = (halfGrid - y) / dy
  | dy < 0 = (halfGrid + y) / (-dy)
  | otherwise = 999999

distanceToBody :: Point -> Snake -> (Float, Float) -> Float
distanceToBody pos snake dir =
  let distances = map (\seg -> distanceToPoint pos seg dir) (tail snake)
  in if null distances then 999999 else minimum distances

wouldCollide :: Point -> Snake -> Float -> Bool
wouldCollide (x, y) snake halfGrid =
  abs x > halfGrid || abs y > halfGrid || (x, y) `elem` tail snake

wouldCollideOrTrap :: Point -> Direction -> Snake -> Float -> Bool
wouldCollideOrTrap pos@(x, y) dir snake halfGrid
  | wouldCollide pos snake halfGrid = True
  | otherwise = 
      let possibleMoves = [(x, y+20), (x, y-20), (x-20, y), (x+20, y)]
          validMoves = filter (\p -> not (wouldCollide p snake halfGrid)) possibleMoves
      in length validMoves < 2