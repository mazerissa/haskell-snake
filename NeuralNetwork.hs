{-# LANGUAGE RecordWildCards #-}

module NeuralNetwork
  ( Network(..)
  , Layer(..)
  , randomNetwork
  , predict
  , sigmoid
  ) where

import System.Random (Random, randomR, mkStdGen, StdGen)

data Layer = Layer
  { weights :: [[Double]]
  , biases :: [Double]
  } deriving (Show)

data Network = Network
  { layers :: [Layer]
  } deriving (Show)

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (-x))

sigmoidVec :: [Double] -> [Double]
sigmoidVec = map sigmoid

dot :: [Double] -> [Double] -> Double
dot xs ys = sum $ zipWith (*) xs ys

forwardLayer :: Layer -> [Double] -> [Double]
forwardLayer Layer{..} inputs =
  sigmoidVec $ zipWith (+) biases weightedSums
  where
    weightedSums = map (`dot` inputs) weights

predict :: Network -> [Double] -> [Double]
predict Network{..} inputs = foldl (flip forwardLayer) inputs layers

randomWeights :: StdGen -> Int -> Int -> ([[Double]], StdGen)
randomWeights gen numNeurons numInputs =
  let (ws, gen') = go gen numNeurons []
  in (ws, gen')
  where
    go g 0 acc = (reverse acc, g)
    go g n acc =
      let (row, g') = randomRow g numInputs []
      in go g' (n - 1) (row : acc)
    
    randomRow g 0 acc = (reverse acc, g)
    randomRow g m acc =
      let (w, g') = randomR (-1.0, 1.0) g
      in randomRow g' (m - 1) (w : acc)

randomBiases :: StdGen -> Int -> ([Double], StdGen)
randomBiases gen n = go gen n []
  where
    go g 0 acc = (reverse acc, g)
    go g m acc =
      let (b, g') = randomR (-1.0, 1.0) g
      in go g' (m - 1) (b : acc)

randomLayer :: StdGen -> Int -> Int -> (Layer, StdGen)
randomLayer gen numNeurons numInputs =
  let (ws, gen') = randomWeights gen numNeurons numInputs
      (bs, gen'') = randomBiases gen' numNeurons
  in (Layer ws bs, gen'')

randomNetwork :: Int -> Network
randomNetwork seed =
  let gen = mkStdGen seed
      (hidden1, gen') = randomLayer gen 32 32
      (hidden2, gen'') = randomLayer gen' 24 32
      (hidden3, gen''') = randomLayer gen'' 16 24
      (output, _) = randomLayer gen''' 4 16
  in Network [hidden1, hidden2, hidden3, output]