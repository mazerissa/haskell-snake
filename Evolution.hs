{-# LANGUAGE RecordWildCards #-}

module Evolution
  ( mutateNetwork
  , calculateFitness
  , crossover
  ) where

import NeuralNetwork
import System.Random (Random, randomR, mkStdGen, StdGen)

calculateFitness :: Int -> Int -> Double
calculateFitness score timeAlive =
  let scoreFitness = fromIntegral score * 200.0
      timeFitness = fromIntegral timeAlive * 1.5
      
      efficiency = if timeAlive > 0 
                   then fromIntegral score / fromIntegral timeAlive * 100.0
                   else 0.0
      
      scoreBonus = if score > 5
                   then fromIntegral score ** 1.5 * 10.0
                   else 0.0
      
      survivalPenalty = if timeAlive < 20
                        then -50.0
                        else 0.0
      
      lengthBonus = fromIntegral score * 8.0
      
  in scoreFitness + timeFitness + efficiency + scoreBonus + lengthBonus + survivalPenalty

mutateValue :: StdGen -> Double -> Double -> (Double, StdGen)
mutateValue gen mutationRate value =
  let (r, gen') = randomR (0.0, 1.0) gen
  in if r < mutationRate
     then let (delta, gen'') = randomR (-0.5, 0.5) gen'
          in (value + delta, gen'')
     else (value, gen')

mutateList :: StdGen -> Double -> [Double] -> ([Double], StdGen)
mutateList gen mutationRate values = go gen values []
  where
    go g [] acc = (reverse acc, g)
    go g (v:vs) acc =
      let (v', g') = mutateValue g mutationRate v
      in go g' vs (v' : acc)

mutateMatrix :: StdGen -> Double -> [[Double]] -> ([[Double]], StdGen)
mutateMatrix gen mutationRate matrix = go gen matrix []
  where
    go g [] acc = (reverse acc, g)
    go g (row:rows) acc =
      let (row', g') = mutateList g mutationRate row
      in go g' rows (row' : acc)

mutateLayer :: StdGen -> Double -> Layer -> (Layer, StdGen)
mutateLayer gen mutationRate Layer{..} =
  let (ws', gen') = mutateMatrix gen mutationRate weights
      (bs', gen'') = mutateList gen' mutationRate biases
  in (Layer ws' bs', gen'')

mutateNetwork :: Int -> Double -> Network -> Network
mutateNetwork seed mutationRate Network{..} =
  let gen = mkStdGen seed
      (layers', _) = go gen layers []
  in Network layers'
  where
    go g [] acc = (reverse acc, g)
    go g (l:ls) acc =
      let (l', g') = mutateLayer g mutationRate l
      in go g' ls (l' : acc)

crossover :: Int -> Network -> Network -> Network
crossover seed (Network ls1) (Network ls2) =
  let gen = mkStdGen seed
      (newLayers, _) = go gen ls1 ls2 []
  in Network newLayers
  where
    go g [] [] acc = (reverse acc, g)
    go g (Layer w1 b1 : rest1) (Layer w2 b2 : rest2) acc =
      let (wCross, g') = crossoverMatrix g w1 w2
          (bCross, g'') = crossoverList g' b1 b2
      in go g'' rest1 rest2 (Layer wCross bCross : acc)
    go g _ _ acc = (reverse acc, g)
    
    crossoverMatrix g m1 m2 = go' g m1 m2 []
      where
        go' gen [] [] a = (reverse a, gen)
        go' gen (r1:rs1) (r2:rs2) a =
          let (r, gen') = crossoverList gen r1 r2
          in go' gen' rs1 rs2 (r : a)
        go' gen _ _ a = (reverse a, gen)
    
    crossoverList g l1 l2 = go'' g l1 l2 []
      where
        go'' gen [] [] a = (reverse a, gen)
        go'' gen (v1:vs1) (v2:vs2) a =
          let (choice, gen') = randomR (0 :: Int, 1) gen
              v = if choice == 0 then v1 else v2
          in go'' gen' vs1 vs2 (v : a)
        go'' gen _ _ a = (reverse a, gen)