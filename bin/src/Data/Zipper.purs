module Data.Zipper where

import Prelude

import Data.Array as Array
import Data.List (List(..))
import Data.List as List

data Zipper a = Zipper (List a) (List a)

derive instance Functor Zipper 
derive instance Eq a => Eq (Zipper a)

instance Show a => Show (Zipper a) where
  show (Zipper l r) = "(Zipper " 
    <> show l 
    <> " " 
    <> show r 
    <> ")"

empty :: forall a. Zipper a 
empty = Zipper Nil Nil

cons :: forall a. a -> Zipper a -> Zipper a
cons a (Zipper l r) = Zipper (Cons a l) r  

delete :: forall a. Zipper a -> Zipper a 
delete zp@(Zipper l r) = case l of 
  Cons _ tl -> Zipper tl r
  Nil -> zp 

moveLeft :: forall a. Zipper a -> Zipper a 
moveLeft z@(Zipper ls rs) = case ls of 
  Cons lhd ltl -> Zipper ltl (Cons lhd rs)
  _ -> z

moveRight :: forall a. Zipper a -> Zipper a 
moveRight z@(Zipper ls rs) = case rs of 
  Cons rhd rtl -> Zipper (Cons rhd ls) rtl 
  _ -> z 

isLast :: forall a. Zipper a -> Boolean
isLast (Zipper _ r) = List.null r

right :: forall a. Zipper a -> List a 
right (Zipper _ r) = r

left :: forall a. Zipper a -> List a 
left (Zipper l _) = l

length :: forall a. Zipper a -> Int 
length (Zipper l r) = List.length l + List.length r

toArray :: forall a. Zipper a -> Array a 
toArray (Zipper ls rs) = List.foldl (flip Array.cons) (Array.fromFoldable rs) ls