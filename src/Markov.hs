module Markov (
    Chain,
    NodeEdges,
    Markov.empty,
    register,
    registerMany,
    fromList,
    predict,
    predictSequence
) where

import Data.Map ( Map, empty, findWithDefault, insert )



-- | A Markov chain.
type Chain a = Map a (NodeEdges a)

-- | The node edges of a node in a chain.
type NodeEdges a = Map a Int



-- | Constructs an empty chain.
empty :: Chain a
empty = Data.Map.empty

-- | Gets the edges of a node.
nodeEdges :: Ord a => a -> Chain a -> NodeEdges a
nodeEdges = findWithDefault Data.Map.empty

-- | Registers a node and a connected node in a chain.
register :: Ord a => a -> a -> Chain a -> Chain a
register current next chain =
    let edges = nodeEdges current chain in
    let chance = findWithDefault 0 next edges + 1 in
    let newEdges = insert next chance edges in
    insert current newEdges chain

-- | Registers a list of nodes in a chain.
registerMany :: Ord a => [a] -> Chain a -> Chain a
registerMany (current:next:xs) chain = registerMany (next : xs) (register current next chain)
registerMany _ chain = chain

-- | Constructs a chain from a list.
fromList :: Ord a => [a] -> Chain a
fromList xs = registerMany xs Markov.empty

-- | Predicts the next value in a chain based on the current value.
predict :: a -> Chain a -> a
predict current chain = error "Not implemented"

-- | Predicts a sequence of values in a chain.
predictSequence :: a -> Chain a -> [a]
predictSequence current chain = current : predictSequence (predict current chain) chain
