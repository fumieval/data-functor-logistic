{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Functor.Logistic
  ( Logistic(..)
  , setters
  ) where

import Data.Distributive
import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Functor.Compose
import Data.Functor.Product
import Data.Proxy
import Data.Complex

class Functor t => Logistic t where
  deliver :: Contravariant f => f (t a -> t a) -> t (f (a -> a))

instance Logistic Identity where
  deliver f = Identity (contramap fmap f)

instance Logistic Proxy where
  deliver _ = Proxy

instance (Logistic f, Logistic g) => Logistic (Product f g) where
  deliver f = Pair
    (deliver (contramap (\u (Pair a b) -> Pair (u a) b) f))
    (deliver (contramap (\u (Pair a b) -> Pair a (u b)) f))

instance (Logistic f, Logistic g, Applicative f, Traversable g, Distributive g) => Logistic (Compose f g) where
  deliver f = Compose
    $ fmap getCompose
    $ deliver
    $ Compose
    $ deliver
    $ contramap go f
    where
      go p = Compose . sequenceA . p . distribute . getCompose

instance Logistic Complex where
  deliver f
    = contramap (\g (a :+ b) -> g a :+ b) f
    :+ contramap (\g (a :+ b) -> a :+ g b) f

setters :: Logistic t => t ((a -> a) -> t a -> t a)
setters = getOp <$> deliver (Op id)
{-# INLINE setters #-}
