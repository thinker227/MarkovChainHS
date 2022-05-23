module Markov (
    Chain,
    NodeEdges,
    register,
    registerMany,
    Markov.empty,
    fromList,
    predict,
    predictStd,
    predictSequence,
    predictSequenceStd
) where

import Data.Map ( Map, empty, findWithDefault, insert, toList )
import System.Random ( Random(randomR), RandomGen, newStdGen )



-- | A Markov chain.
type Chain a = Map a (NodeEdges a)

-- | The node edges of a node in a chain.
type NodeEdges a = Map a Int



-- | Gets the edges of a node.
nodeEdges :: Ord a => a -> Chain a -> NodeEdges a
nodeEdges = findWithDefault Data.Map.empty

-- | Registers a node and a connected node in a chain.
register :: Ord a => a -> a -> Chain a -> Chain a
register current next chain = let
    edges = nodeEdges current chain
    chance = findWithDefault 0 next edges + 1
    newEdges = insert next chance edges
    in insert current newEdges chain

-- | Registers a list of nodes in a chain.
registerMany :: Ord a => [a] -> Chain a -> Chain a
registerMany (current:next:xs) chain = registerMany (next : xs) (register current next chain)
registerMany _ chain = chain

-- | Constructs an empty chain.
empty :: Chain a
empty = Data.Map.empty

-- | Constructs a chain from a list.
fromList :: Ord a => [a] -> Chain a
fromList xs = registerMany xs Markov.empty

getPrediction :: Int -> [(a, Int)] -> Int -> a
getPrediction target ((val, chance) : xs) accum =
    let current = accum + chance in
    if current >= target
        then val
        else getPrediction target xs current
getPrediction _ _ _ = error "List is empty"

-- | Predicts the next value in a chain based on the current value and a `RandomGen`.
predict :: (Ord a, RandomGen g) => a -> g -> Chain a -> (a, g)
predict current gen chain = let
    edges = findWithDefault (error "Node doesn't exist") current chain
    edgesList = toList edges
    total = sum $ map snd edgesList
    (target, gen') = randomR (0, total) gen
    prediction = getPrediction target edgesList 0
    in (prediction, gen')

-- | Predicts the next value in a chain based on the current value and a default `RandomGen`.
predictStd :: (Ord a) => a -> Chain a -> IO a
predictStd current chain = do
    gen <- newStdGen
    return $ fst $ predict current gen chain

-- | Predicts a sequence of values in a chain.
predictSequence :: (Ord a, RandomGen g) => a -> g -> Chain a -> [a]
predictSequence current gen chain = let
    (prediction, gen') = predict current gen chain
    in current : predictSequence prediction gen' chain

-- | Predicts a sequence of values in a chain using a default `RandomGen`.
predictSequenceStd :: (Ord a) => a -> Chain a -> IO [a]
predictSequenceStd current chain = do
    gen <- newStdGen
    return $ predictSequence current gen chain
