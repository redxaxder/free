










{-# LANGUAGE DeriveFoldable
           , DeriveFunctor
           , Rank2Types
           , TypeOperators #-}

module Free.Stuff where

import Prelude hiding (List, (++))
import Data.Monoid
import Data.Foldable
import Control.Monad

 --  ______                _____ _          __  __
 -- |  ____|              / ____| |        / _|/ _|
 -- | |__ _ __ ___  ___  | (___ | |_ _   _| |_| |_
 -- |  __| '__/ _ \/ _ \  \___ \| __| | | |  _|  _|
 -- | |  | | |  __/  __/  ____) | |_| |_| | | | |
 -- |_|  |_|  \___|\___| |_____/ \__|\__,_|_| |_|




















data List a = Cons a !(List a) | Nil deriving (Foldable, Functor)

(++) :: List a -> List a -> List a
Nil ++ ys = ys
(Cons x xs) ++ ys = Cons x (xs ++ ys)

instance Monoid (List a) where
  mempty = Nil
  mappend = (++)



data Two = One | Two








{- "F is free monoid over X" means...
 -
 - There is a function unit :: X -> F and
 -
 - given any monoid M and any function f :: X -> M
 -
 - there is a unique monoid morphism f' :: F -> M
 - satisfying the property
 - f' . unit = f
 -
 -
 -
 - A monoid morphism is a function f satisfying the laws
 -
 - - f mempty = mempty
 - - f (x <> y) = f x <> f y
 -
 -}






unit :: a -> List a
unit x = Cons x Nil






instance Monoid Int where
  mempty = 0
  mappend = (+)

ex1 :: Two -> Int
ex1 One = 3
ex1 Two = 7


ex1' :: List Two -> Int
ex1' Nil = 0
ex1' (Cons x xs) = ex1 x <> ex1' xs







ex2 :: (Monoid m) => (Two -> m) -> List Two -> m
ex2 f Nil = mempty
ex2 f (Cons x xs) = f x <> ex2 f xs







ex3 :: (Monoid m) => (Two -> m) -> List Two -> m
ex3 = foldMap

















data Point = P Int Int

instance Monoid Point where
  mempty = P 0 0
  mappend (P a1 a2) (P b1 b2) = P (a1 + b1) (a2 + b2)

class (Monoid g) => Group g where
  inverse :: g -> g

instance Group Point where
  inverse (P x y) = P (-x) (-y)











class (Group m) => Module m where
  (.*) :: Int -> m -> m
infixr 7 .* -- higher precedence than <>

{- Module Laws:
 -
 - Distributivity:
 -    (s <> t) .* m = (s .* m) <> (s .* m)
 -
 - Associativity:
 -    (s .* (t .* m)) = (s * t) .* m
 -
 - Unitary:
 -    1 .* m = m
 -}

instance Module Point where
 s .* (P x y) = P (s * x) (s * y)







{- "F is a free module over X" means...
 -
 -  There is a function unit :: X -> F and
 -
 -  given any module M and any function f :: X -> M
 -
 -  there is a unique module morphism  f' :: F -> M
 -  satisfying the property
 -  f' . unit = f
 -
 -
 -
 -
 -  A module morphism is monoid morphism satisfying the additional law
 -   - Commutes with scalar multiplicaiton:
 -   - s .* (f m) = f (s .* m)
 -
 -}



unit2 :: Two -> Point
unit2 One = P 1 0
unit2 Two = P 0 1






instance Group Int where
  inverse x = -x
instance Module Int where
  x .* y = x * y





ey1 :: Two -> Int
ey1 One = 3
ey1 Two = 7

ey1' :: Point -> Int
ey1' (P x y) = x .* (ey1 One) <> y .* (ey1 Two)






{-
 - kindaLikeFoldMap :: (Module m) => (a -> m) -> ??? -> m
 -}












newtype FreeMonoid a = FreeMonoid { foldMonoid :: forall m. Monoid m => (a -> m) -> m }

instance Foldable FreeMonoid where
  foldMap = flip foldMonoid

unit3 :: a -> FreeMonoid a
unit3 x = FreeMonoid ($x)









instance Monoid (FreeMonoid a) where
  mempty = FreeMonoid (const mempty)
  mappend x y = FreeMonoid $ \f ->
    foldMonoid x f <> foldMonoid y f












newtype FreeModule a = FreeModule { foldModule :: forall m.  Module m => (a -> m) -> m }

unit4 :: a -> FreeModule a
unit4 x = FreeModule ($x)

kindaLikeFoldMap :: Module m => (a -> m) -> FreeModule a -> m
kindaLikeFoldMap = flip foldModule






instance Monoid (FreeModule a) where
  mempty = FreeModule (const mempty)
  mappend x y = FreeModule $ \f ->
    foldModule x f <> foldModule y f

instance Group (FreeModule a) where
  inverse x = FreeModule $ \f ->
    inverse (foldModule x f)

instance Module (FreeModule a) where
  s .* x = FreeModule $ \f ->
    s .* (foldModule x f)







-- A natural transofrmation between functors
type (~>) f g = forall a. f a -> g a



{- "F is a free monad over X" means...
 -
 -  There is a natural transformation unit :: X ~> F and
 -
 -  given any module M and any natural transformation f :: X ~> M
 -
 -  there is a unique monad morphism  f' :: F ~> M
 -  satisfying the property
 -  f' . unit = f
 -
 -
 -
 -
 -  A monad morphism is natural transformation n satisfying the laws
 -
 - - n (return x)    =    return x
 - - n (m >>= f)     =    n m >>= (n . f)
 -
 -
 -  Control.Monad.Morph has two alternative phrasings of the second law:
 -
 - - n $ do x <- m    =    do x <- n m
 -          f x               n (f x)
 -
 - - n . (f >=> g)    =    n . f >=> n . p
 -}



kindaLikeFoldMapButForMonads :: (Functor f, Monad m) => (f ~> m) -> (FreeMonad f ~> m)
 -- this is named foldFree in Control.Monad.Free






newtype FreeMonad f a = FreeMonad { foldFreeMonad :: forall m . Monad m => (f ~> m) -> m a }
kindaLikeFoldMapButForMonads nat fm = foldFreeMonad fm nat




unit5 :: f ~> FreeMonad f
unit5 fx = FreeMonad ($fx)










instance Functor f => Functor (FreeMonad f) where
  fmap f fm = FreeMonad $ \nat ->
    fmap f (foldFreeMonad fm nat)

instance Functor f => Monad (FreeMonad f) where
  return x = FreeMonad $ \nat ->
    return x
  fm >>= f = FreeMonad $ \nat -> do
     a <- foldFreeMonad fm nat
     foldFreeMonad (f a) nat


instance Functor f => Applicative (FreeMonad f) where
  pure = return
  (<*>) = ap












{- "F is a free applicative over X" means...
 -
 -  There is a natural transformation unit :: X ~> F and
 -
 -  given any module M and any natural transformation f :: X ~> M
 -
 -  there is a unique applicative morphism  f' :: F ~> M
 -  satisfying the property
 -  f' . unit = f
 -
 -
 -
 -
 -  An applicative morphism is (probably?) natural transformation n satisfying the laws
 -
 - - n (pure x)      =    pure x
 -
 - - n (f <*> x)     =    (n f <*> n x)
 -}






kindaLikeFoldMapButForApplicatives :: Applicative g => (f ~> g) -> (FreeApplicative f ~> g)
-- this is named runAp in Control.Applicative.Free





newtype FreeApplicative f a = FreeApplicative {
  foldFreeApplicative :: forall g. Applicative g => (f ~> g) -> g a
  }

kindaLikeFoldMapButForApplicatives nat fa = foldFreeApplicative fa nat

unit6 :: Functor f => f ~> FreeApplicative f
unit6 fx = FreeApplicative ($fx)




instance Functor f => Functor (FreeApplicative f) where
  fmap f fa = FreeApplicative $ \nat ->
    fmap f (foldFreeApplicative fa nat)

instance Functor f => Applicative (FreeApplicative f) where
  pure x = FreeApplicative $ \nat -> pure x
  f <*> x = FreeApplicative $ \nat ->
    foldFreeApplicative f nat <*> foldFreeApplicative x nat









{- ADJOINT FUNCTORS
 -
 -   Given two categories, C and D, an Adjunction is a pair of functors
 -
 -   F: D -> C
 -   G: C -> D
 -
 -   along with two natural transformations
 -
 -   unit : Identity_C ~> G . F
 -   counit : F . G ~> Identity_D
 -
 -
 -
 -  Given an adjunction, a theorem gives us the following invertible function:
 -  phi   :: (F a -> b) -> (a -> G b)
 -  unPhi :: (a -> G b) -> (F a -> b)
 -
 -
 -
 - the foldMap we've been repeatedly defining is
 -
 - foldMap :: (a -> G b) -> (G (F a) -> G b)
 - foldMap f = fmap_G (unPhi f)
 -}





{-  some functors do not correspond to datatypes with Functor instances.
 -
 -
 -  (ex: unIdentity with unfmap is a functor, but not a Functor.
 -
 -data Identity a = Identity { unIdentity :: a }  deriving Functor
 -unfmap :: (Identity a -> Identity b) -> a -> b
 -unfmap f = unIdentity . f . Identity
 -
 -  Similarly, id :: a -> b with id :: (a -> b) -> (a -> b) is not a Functor)
 -}



 {- If you have an adjunction (F, G), then...
 -
 -
 - G . F is a monad and G . F is a comonad.
 - return = unit
 - (>>=f) = fmap_G (unPhi f) -- that looks familiar...
 -}




