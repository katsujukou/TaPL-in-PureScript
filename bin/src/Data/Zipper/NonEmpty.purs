module Data.Zipper.NonEmpty where

import Prelude

import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Zipper as Z

data NonEmptyZipper a = NonEmptyZipper (List a) a (List a)

derive instance Functor NonEmptyZipper 
derive instance Eq a => Eq (NonEmptyZipper a)

instance Show a => Show (NonEmptyZipper a) where
  show (NonEmptyZipper l c r) = "(NonEmptyZipper " 
    <> show l 
    <> " " 
    <> show c 
    <> " "
    <> show r 
    <> ")"

fromZipper :: forall a. Z.Zipper a -> Maybe (NonEmptyZipper a)
fromZipper (Z.Zipper l r) = case l, r of 
  Cons hd tl, _ -> Just $ NonEmptyZipper tl hd r
  Nil, Cons hd tl -> Just $ NonEmptyZipper Nil hd tl
  Nil, Nil -> Nothing

toZipper :: forall a. NonEmptyZipper a -> Z.Zipper a 
toZipper (NonEmptyZipper l c r) = Z.Zipper (Cons c l) r

toArray :: forall a. NonEmptyZipper a -> Array a
toArray = toZipper >>> Z.toArray

right :: forall a. NonEmptyZipper a -> List a 
right (NonEmptyZipper _ _ r) = r

singleton :: forall a. a -> NonEmptyZipper a 
singleton a = NonEmptyZipper Nil a Nil 

extract :: forall a. NonEmptyZipper a -> a 
extract (NonEmptyZipper _ c _) = c 

left :: forall a. NonEmptyZipper a -> List a 
left (NonEmptyZipper l _ _) = l 

isLast :: forall a. NonEmptyZipper a -> Boolean
isLast (NonEmptyZipper _ _ r) = L.null r 

cons :: forall a. a -> NonEmptyZipper a -> NonEmptyZipper a
cons x (NonEmptyZipper l c r) = NonEmptyZipper (Cons c l) x r 

delete :: forall a. NonEmptyZipper a -> Z.Zipper a 
delete (NonEmptyZipper l _ r) = Z.Zipper l r

update :: forall a. (a -> a) -> NonEmptyZipper a -> NonEmptyZipper a 
update f (NonEmptyZipper l c r) = NonEmptyZipper l (f c) r

delete' :: forall a. NonEmptyZipper a -> Maybe (NonEmptyZipper a)
delete' = delete >>> fromZipper

moveRight :: forall a. NonEmptyZipper a -> NonEmptyZipper a 
moveRight z@(NonEmptyZipper ls c rs) = case rs of 
  Cons rhd rtl -> NonEmptyZipper (Cons c ls) rhd rtl 
  _ -> z 

moveLeft :: forall a. NonEmptyZipper a -> NonEmptyZipper a 
moveLeft z@(NonEmptyZipper ls c rs) = case ls of 
  Cons lhd ltl -> NonEmptyZipper ltl lhd (Cons c rs)
  _ -> z

moveEnd :: forall a. NonEmptyZipper a -> NonEmptyZipper a 
moveEnd z@(NonEmptyZipper l c r) = case L.unsnoc (Cons c r) of
  Nothing -> z 
  Just {init, last} -> NonEmptyZipper (L.foldl (flip Cons) l init) (last) (Nil) 