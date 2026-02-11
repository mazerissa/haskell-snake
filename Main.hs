{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Data.Point as G
import GameLogic
import NeuralNetwork
import Evolution
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import System.Random (randomR, mkStdGen)

data Individual = Individual
  { indNetwork :: Network
  , indFitness :: Double
  , indGamesPlayed :: Int
  } deriving (Show)

data World = World
  { currentGame :: GameState
  , currentNetwork :: Network
  , population :: [Individual]
  , generation :: Int
  , bestFitness :: Double
  , currentFitness :: Double
  , populationSize :: Int
  , currentIndividualIdx :: Int
  , generationGames :: Int  
  }

popSize :: Int
popSize = 30  

gamesPerIndividual :: Int
gamesPerIndividual = 5  

baseMutationRate :: Double
baseMutationRate = 0.2  

eliteCount :: Int
eliteCount = 3  

adaptiveMutationRate :: Int -> Double -> Double
adaptiveMutationRate gen bestFit
  | gen < 10 = 0.25  
  | gen < 30 = 0.15  
  | bestFit < 200 = 0.12  
  | otherwise = 0.08  

initialWorld :: World
initialWorld = 
  let pop = [Individual (randomNetwork (42 + i * 100)) 0.0 0 | i <- [0..popSize-1]]
      firstNet = indNetwork (head pop)
  in World
      { currentGame = initialGameState 42
      , currentNetwork = firstNet
      , population = pop
      , generation = 1
      , bestFitness = 0
      , currentFitness = 0
      , populationSize = popSize
      , currentIndividualIdx = 0
      , generationGames = 0
      }

window :: Display
window = InWindow "Snake AI" (800, 600) (100, 100)

background :: Color
background = black

fps :: Int
fps = 20  

main :: IO ()
main = playIO window background fps initialWorld drawWorld handleInput updateWorld

drawWorld :: World -> IO Picture
drawWorld World{..} = do
  let GameState{..} = currentGame
      snakePic = drawSnake snake
      foodPic = drawFood food
      currentMutRate = adaptiveMutationRate generation bestFitness
      infoPic = drawInfo generation score timeAlive bestFitness currentFitness isAlive
      popPic = drawPopulationInfo currentIndividualIdx populationSize generationGames (populationSize * gamesPerIndividual) currentMutRate
  return $ pictures [snakePic, foodPic, infoPic, popPic]

drawSnake :: Snake -> Picture
drawSnake snake = pictures $ zipWith drawSegment snake [0..]
  where
    drawSegment (x, y) idx = 
      let intensity = 1.0 - (fromIntegral idx / fromIntegral (length snake)) * 0.5
          segColor = makeColor 0 intensity 0 1.0
      in translate x y $ color segColor $ rectangleSolid 18 18

drawFood :: GameLogic.Point -> Picture
drawFood (x, y) = translate x y $ pictures
  [ color red $ circleSolid 10
  , color orange $ circleSolid 7
  ]

drawInfo :: Int -> Int -> Int -> Double -> Double -> Bool -> Picture
drawInfo gen score time best current alive =
  translate (-380) 250 $ scale 0.18 0.18 $ pictures
    [ translate 0 0 $ color white $ text $ "Generation: " ++ show gen
    , translate 0 (-150) $ color yellow $ text $ "Score: " ++ show score
    , translate 0 (-300) $ color white $ text $ "Time: " ++ show time
    , translate 0 (-450) $ color green $ text $ "Best: " ++ show (round best :: Int)
    , translate 0 (-600) $ color cyan $ text $ "Current: " ++ show (round current :: Int)
    , translate 0 (-750) $ color (if alive then green else red) $ 
        text $ if alive then "ALIVE" else "DEAD"
    ]

drawPopulationInfo :: Int -> Int -> Int -> Int -> Double -> Picture
drawPopulationInfo currentIdx pSize gamesCompleted totalGames currentMutRate =
  translate (-380) (-200) $ scale 0.18 0.18 $ pictures
    [ translate 0 0 $ color white $ text $ "Population: " ++ show pSize
    , translate 0 (-150) $ color cyan $ text $ "Individual: " ++ show (currentIdx + 1) ++ "/" ++ show pSize
    , translate 0 (-300) $ color yellow $ text $ "Progress: " ++ show gamesCompleted ++ "/" ++ show totalGames
    , translate 0 (-450) $ color green $ text $ "Mutation: " ++ show (round (currentMutRate * 100) :: Int) ++ "%"
    ]

handleInput :: Event -> World -> IO World
handleInput _ world = return world

updateWorld :: Float -> World -> IO World
updateWorld _ world@World{..}
  | isGameOver currentGame = do
      let gameFitness = calculateFitness (score currentGame) (timeAlive currentGame)
      
      let currentInd = population !! currentIndividualIdx
          updatedInd = currentInd 
            { indFitness = gameFitness + indFitness currentInd
            , indGamesPlayed = indGamesPlayed currentInd + 1
            }
          updatedPop = take currentIndividualIdx population ++ 
                       [updatedInd] ++ 
                       drop (currentIndividualIdx + 1) population
      
      let gamesForThisInd = indGamesPlayed updatedInd
          needsNewIndividual = gamesForThisInd >= gamesPerIndividual
          nextIdx = if needsNewIndividual 
                    then currentIndividualIdx + 1 
                    else currentIndividualIdx
          newGenGames = generationGames + 1
      
      if nextIdx >= populationSize then do
        let avgFitnessPop = map (\ind -> ind { indFitness = indFitness ind / fromIntegral (indGamesPlayed ind) }) updatedPop
            sortedPop = sortBy (comparing (negate . indFitness)) avgFitnessPop
            newBest = maximum [bestFitness, indFitness (head sortedPop)]
            
            currentMutationRate = adaptiveMutationRate generation newBest
            
            elite = take eliteCount sortedPop
            
            newPop = elite ++ evolvePopulation (generation + 1) currentMutationRate sortedPop (populationSize - eliteCount)
            
            firstNet = indNetwork (head newPop)
            newGame = initialGameState ((generation + 1) * 1000)
        
        return $ World
          { currentGame = newGame
          , currentNetwork = firstNet
          , population = newPop
          , generation = generation + 1
          , bestFitness = newBest
          , currentFitness = 0
          , populationSize = populationSize
          , currentIndividualIdx = 0
          , generationGames = 0
          }
      else do
        let nextNet = indNetwork (updatedPop !! nextIdx)
            newGame = initialGameState ((generation * 1000) + nextIdx * 100 + gamesForThisInd)
        
        return $ world
          { currentGame = newGame
          , currentNetwork = nextNet
          , population = updatedPop
          , currentIndividualIdx = nextIdx
          , generationGames = newGenGames
          , currentFitness = 0
          }
  
  | otherwise = do
      let inputs = getInputsForNN currentGame
      let outputs = predict currentNetwork inputs
      let dir = chooseDirection outputs
      let newGameState = updateGame dir currentGame
          gameFitness = calculateFitness (score newGameState) (timeAlive newGameState)
      
      return $ world
        { currentGame = newGameState
        , currentFitness = gameFitness
        }

evolvePopulation :: Int -> Double -> [Individual] -> Int -> [Individual]
evolvePopulation gen mutRate sorted count =
  let seed = gen * 12345
  in map (createOffspring seed mutRate sorted) [0..count-1]

createOffspring :: Int -> Double -> [Individual] -> Int -> Individual
createOffspring baseSeed mutRate sorted idx =
  let seed = baseSeed + idx * 100
      gen = mkStdGen seed
      (parent1Idx, gen') = randomR (0, min 7 (length sorted - 1)) gen
      (parent2Idx, gen'') = randomR (0, min 7 (length sorted - 1)) gen'
      
      parent1 = indNetwork (sorted !! parent1Idx)
      parent2 = indNetwork (sorted !! parent2Idx)
      
      (doCrossover, gen''') = randomR (0.0, 1.0 :: Double) gen''
      crossed = if doCrossover < 0.75
                then crossover seed parent1 parent2
                else parent1
      
      mutated = mutateNetwork (seed + 999) mutRate crossed
  in Individual mutated 0.0 0

chooseDirection :: [Double] -> Direction
chooseDirection outputs =
  let indexed = zip outputs [DUp, DDown, DLeft, DRight]
      (_, dir) = maximumBy (comparing fst) indexed
  in dir