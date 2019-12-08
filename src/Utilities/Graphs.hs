{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Utilities.Graphs where

import Data.Sequence ()
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Control.Monad (guard)
import Data.List (sortBy)
import Control.Applicative ((<|>))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

type Distances a = M.Map (Vertex a) Int
type Prevs a = M.Map (Vertex a) (Vertex a)

newtype Vertex a = Keyed a deriving (Ord, Eq, Show)

class Vertices g where
    vertices :: Ord a => g a -> S.Set (Vertex a)

class Vertices g => Graph g where
    neighbours :: Ord a => g a -> Vertex a -> S.Set (Int, Vertex a)

class Vertices g => EquallyWeightedGraph g where
    equalNeighbours :: Ord a => g a -> Vertex a -> S.Set (Vertex a)

instance {-# OVERLAPPABLE #-} (EquallyWeightedGraph g, Vertices g) => Graph g where
    neighbours g v = S.map ((, ) 1) $ equalNeighbours g v

newtype ListGraph a = EquallyWeighted [(Vertex a, Vertex a)] deriving Show

instance Vertices ListGraph where
    vertices (EquallyWeighted vs) = S.fromList (map fst vs ++ map snd vs)

instance EquallyWeightedGraph ListGraph where
    equalNeighbours (EquallyWeighted vs) v =
        S.fromList [n | (v', n) <- vs, v' ==v]

newtype MapGraph a = MapGraph { toMap :: M.Map (Vertex a) (S.Set (Int, Vertex a)) } deriving Show

newtype EqualMapGraph a = EqualMapGraph { toEqualMap :: M.Map (Vertex a) (S.Set (Vertex a)) }

listGraphToMapGraph :: Ord a => ListGraph a -> MapGraph a
listGraphToMapGraph (EquallyWeighted pairs) =
    MapGraph $ foldr insert M.empty pairs
        where
            insert (a, b) m =
                M.alter (\case
                            Nothing -> Just $ S.singleton (1, b)
                            Just s -> Just $ S.insert (1, b) s
                        ) a m

listGraphToEqualMapGraph :: Ord a => ListGraph a -> EqualMapGraph a
listGraphToEqualMapGraph =
    EqualMapGraph . M.map (S.map snd) . toMap . listGraphToMapGraph

instance Vertices MapGraph where
    vertices (MapGraph vs) =
        S.unions (M.keysSet vs:(S.map snd <$> M.elems vs))

instance Vertices EqualMapGraph where
    vertices (EqualMapGraph vs) =
        S.unions (M.keysSet vs:M.elems vs)

instance Graph MapGraph where
    neighbours =
        flip (M.findWithDefault S.empty) . toMap

instance EquallyWeightedGraph EqualMapGraph where
    equalNeighbours =
        flip (M.findWithDefault S.empty) . toEqualMap

{-
dijkstra :: (Graph g, Ord a) => g a -> Vertex a -> (Distances a, Prevs a)
dijkstra graph source = 
    aux (vertices graph) (M.singleton source 0) M.empty

    where
        aux q dist prev = fromMaybe (dist, prev) $ do
            (_, u) <- S.lookupMin $ S.map (\v -> (M.findWithDefault maxBound v dist, v)) q
            pure
                $ uncurry (aux (S.delete u q))
                $ foldr (insertShorter u) (dist, prev)
                $ neighbours graph u
        
        insertShorter u (length, v) (dist, prev) = fromMaybe (dist, prev) $ do
            uD <- M.lookup u dist
            let alt = uD + length
            guard $ alt < M.findWithDefault maxBound v dist
-}
