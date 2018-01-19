{-# OPTIONS -Wall #-}

module Dress where

import Data.Graph (Graph, buildG)
import Control.Monad (forM_, unless)
import System.Environment (getArgs)

data Item
  = Backpack
  | Boots
  | Sneakers
  | Corset
  | CycleClips
  | Glasses
  | Hanky
  | Hat
  | Helmet
  | Hoodie
  | Jacket
  | LongGloves
  | Pants
  | Poncho
  | SamBrowne
  | Keys
  | Scarf
  | Shirt
  | ShortGloves
  | Socks
  | TShirt
  | Tie
  | Undershirt
  | Underwear
  | Vest
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

main :: IO ()
main = do
  exclude <- fmap (map read) getArgs
  putStrLn "strict digraph Dress {"
  forM_ constraints (\(x,y) ->
    unless (x `elem` exclude || y `elem` exclude)
           (putStrLn ("  " ++ show x ++ " -> " ++ show y ++ ";")))
  if Corset `elem` exclude
     then putStrLn "  { rank=max; Boots; Sneakers; LongGloves; }"
     else putStrLn "  { rank=same; Boots; Sneakers; }"
  putStrLn "}"

graph :: Graph
graph = buildG (fromEnum (minBound :: Item), fromEnum (maxBound :: Item))
               [ (fromEnum x, fromEnum y) | (x,y) <- constraints ]

constraints :: [(Item, Item)]
constraints = concat [foot, ankle, butt, hip, chest, neck, face, wrist, palm, temple, action]

foot, ankle, butt, hip, chest, neck, face, wrist, palm, temple, action :: [(Item, Item)]
foot   = order [[Socks],
                [Sneakers, Boots]]
ankle  = order [[Socks],
                [Pants],
                [CycleClips],
                [Boots]]
butt   = order [[Underwear],
                [Pants],
                [Hanky]]
hip    = order [[Undershirt, TShirt, Shirt],
                [Pants],
                [Hoodie, Jacket]]
chest  = order [[Undershirt],
                [TShirt],
                [Shirt, Corset],
                [SamBrowne],
                [Vest],
                [Hoodie],
                [Jacket],
                [Backpack],
                [Poncho]]
neck   = order [[Undershirt, TShirt, Shirt],
                [Tie],
                [SamBrowne],
                [Hoodie],
                [Scarf],
                [Jacket]]
face   = order [[Hat],
                [Helmet],
                [Scarf]]
wrist  = order [[ShortGloves],
                [Hoodie, Jacket]]
palm   = order [[ShortGloves],
                [LongGloves]]
temple = order [[Glasses],
                [Hat, Helmet]]
action = order [[Hoodie],
                [Glasses]]
      ++ order [[Sneakers, Boots],
                [Corset]]
      ++ order [[Sneakers, Boots, Shirt, Tie],
                [LongGloves]]
      ++ order [[Underwear, Pants],
                [Sneakers, Boots, SamBrowne, Keys]]

order :: [[a]] -> [(a, a)]
order []       = []
order (xs:yss) = [ (x,y) | x <- xs, ys <- yss, y <- ys ] ++ order yss
