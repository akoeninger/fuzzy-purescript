module Exercises where

import Prelude

import Control.Applicative
import Control.Apply
import Data.List
import Data.Maybe

combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs

maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd = lift2 (+)

maybeMinus :: Maybe Int -> Maybe Int -> Maybe Int
maybeMinus = lift2 (-)

maybeMult :: Maybe Int -> Maybe Int -> Maybe Int
maybeMult = lift2 (*)

maybeDiv :: Maybe Int -> Maybe Int -> Maybe Int
maybeDiv = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) =  Just <$> x

