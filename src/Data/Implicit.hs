{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

"Data.Implicit" provides both named and unnamed implicit parameters that
support default values (given by the 'Default' class from the @data-default@
package). It makes no use of the @ImplicitParams@ extension and instead
everything is done using type classes.

Here is an example of unnamed implicit parameters:

@
{\-\# LANGUAGE DataKinds, FlexibleContexts #-\}
import "Data.Implicit"

putParam :: 'Implicit_' String => IO ()
putParam = putStrLn $ \"Param was: \" ++ show ('param_' :: String)
@

>>> putParam -- use the default value
Param was ""

>>> 'explicitly_' "hello, world" $ putParam
Param was "hello, world"

Here is an example of named implicit parameters:

@
{\-\# LANGUAGE DataKinds, FlexibleContexts, RankNTypes #-\}
import "Data.Implicit"
import "Data.Proxy"

putFooBar :: ('Implicit' \"foo\" String, 'Implicit' \"bar\" String) => IO ()
putFooBar = do
    putStrLn $ \"Foo was: \" ++ show getFoo
    putStrLn $ \"Bar was: \" ++ show getBar

getFoo :: 'Implicit' \"foo\" String => String
getFoo = 'param' (Proxy :: Proxy \"foo\")

getBar :: 'Implicit' \"bar\" String => String
getBar = 'param' (Proxy :: Proxy \"bar\")

withFoo :: String -> ('Implicit' \"foo\" String => a) -> a
withFoo = 'explicitly' (Proxy :: Proxy \"foo\")

withBar :: String -> ('Implicit' \"bar\" String => a) -> a
withBar = 'explicitly' (Proxy :: Proxy \"bar\")
@

>>> putFooBar
Foo was: ""
Bar was: ""

>>> withFoo "hello, world" putFooBar
Foo was: "hello, world"
Bar was: ""

>>> withBar "goodbye" $ withFoo "hello, world" putFooBar
Foo was: "hello, world"
Bar was: "goodbye"

-}

module Data.Implicit
    ( Implicit
    , param
    , explicitly

    , Implicit_
    , param_
    , explicitly_
    )
where

import           Data.Default (Default, def)
import           GHC.TypeLits (Symbol)
import           GHC.Exts (Any, Constraint)
import           Unsafe.Coerce (unsafeCoerce)


------------------------------------------------------------------------------
class Implicit (s :: Symbol) a where
    _param :: proxy s -> proxy' a -> a


------------------------------------------------------------------------------
instance Default a => Implicit s a where
    _param _ _ = def


------------------------------------------------------------------------------
param :: Implicit s a => proxy s -> a
param p = _param p Proxy


------------------------------------------------------------------------------
explicitly :: proxy s -> a -> (Implicit s a => b) -> b
explicitly = using


------------------------------------------------------------------------------
type Implicit_ = Implicit Any


------------------------------------------------------------------------------
param_ :: Implicit_ a => a
param_ = param (Proxy :: Proxy Any)


------------------------------------------------------------------------------
explicitly_ :: a -> (Implicit_ a => b) -> b
explicitly_ = explicitly (Proxy :: Proxy Any)


-- edwardk is the new oleg ---------------------------------------------------

------------------------------------------------------------------------------
data Proxy a = Proxy


------------------------------------------------------------------------------
newtype Lift s a t = Lift a


------------------------------------------------------------------------------
instance Reifies t a => Implicit s (Lift s a t) where
    _param _ a = Lift $ reflect (peek a)
      where
        peek :: proxy b -> b
        peek _ = undefined


------------------------------------------------------------------------------
reifiedInstance :: Reifies t a :- Implicit s (Lift s a t)
reifiedInstance = Sub Dict


------------------------------------------------------------------------------
using :: proxy s -> a -> (Implicit s a => b) -> b
using (_ :: proxy s) d m = reify d $ \(_ :: Proxy t) -> m \\
    trans
        (unsafeCoerceConstraint :: (Implicit s (Lift s a t) :- Implicit s a))
        reifiedInstance


------------------------------------------------------------------------------
data Dict :: Constraint -> * where
    Dict :: a => Dict a


------------------------------------------------------------------------------
newtype a :- b = Sub (a => Dict b)
infixr 9 :-


------------------------------------------------------------------------------
(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r
infixl 1 \\ -- required comment


------------------------------------------------------------------------------
trans :: (b :- c) -> (a :- b) -> a :- c
trans f g = Sub $ Dict \\ f \\ g


------------------------------------------------------------------------------
unsafeCoerceConstraint :: a :- b
unsafeCoerceConstraint = unsafeCoerce (Sub Dict :: a :- a)


------------------------------------------------------------------------------
type Proxy' (a :: *) = Proxy a


------------------------------------------------------------------------------
class Reifies t a | t -> a where
    reflect :: proxy t -> a


------------------------------------------------------------------------------
newtype Magic a r = Magic (forall t. Reifies t a => Proxy' t -> r)


------------------------------------------------------------------------------
reify :: forall a r. a -> (forall t. Reifies t a => Proxy' t -> r) -> r
reify a k = unsafeCoerce (Magic k :: Magic a r) (const a) Proxy