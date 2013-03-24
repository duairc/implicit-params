{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

"Data.Implicit" provides both named and unnamed implicit parameters that
support default values (given by the 'Default' class from the @data-default@
package). It makes no use of the @ImplicitParams@ extension and instead
everything is done using type classes.

Here is an example of unnamed implicit parameters:

@
{\-\# LANGUAGE FlexibleContexts #-\}
import "Data.Implicit"

putParam :: 'Implicit_' String => IO ()
putParam = putStrLn $ \"Param was: \" ++ show ('param_' :: String)
@

We define @putParam@, which is a simple function which takes an implicit
parameter of type @String@, and prints it to the screen. The 'param_' function
is used to retrieve the unnamed implicit parameter of type @String@ from
@putParam@'s context. The type signature is necessary to force 'param_' to
return a @String@, as this cannot be inferred due to the polymorphism of
@show@.

>>> putParam
Param was ""

This is how we call @putParam@ without specifying its implicit parameters. If
an implicit parameter is left unspecified, its value is defaulted to 'def',
assuming that its type has a 'Default' instance. If not, then it is a type
error not to specify the value of an implicit parameter.

>>> putParam $~ "hello, world"
Param was "hello, world"

The operator '$~' takes a function @f@ and a value to which to set the
homotypic implicit parameter on @f@. It applies the implicit parameter to @f@
and returns the result. There is also a prefix version of @$~@ whose arguments
are flipped called 'setParam_'.

Here is an example of named implicit parameters:

@
{\-\# LANGUAGE DataKinds, FlexibleContexts, RankNTypes #-\}
import "Data.Implicit"
import "Data.Proxy"

putFooBar :: ('Implicit' \"foo\" String, 'Implicit' \"bar\" String) => IO ()
putFooBar = do
    putStrLn $ \"foo was: \" ++ show foo
    putStrLn $ \"bar was: \" ++ show bar

foo :: 'Implicit' \"foo\" String => String
foo = 'param' (Proxy :: Proxy \"foo\")

bar :: 'Implicit' \"bar\" String => String
bar = 'param' (Proxy :: Proxy \"bar\")

setFoo :: String -> ('Implicit' \"foo\" String => a) -> a
setFoo = 'setParam' (Proxy :: Proxy \"foo\")

setBar :: String -> ('Implicit' \"bar\" String => a) -> a
setBar = 'setParam' (Proxy :: Proxy \"bar\")
@

The 'Implicit' constraint is the named equivalent of 'Implicit_'. It takes an
additional argument of kind 'Symbol' (which requires the @DataKinds@
extension; see the "GHC.TypeLits" module) to specify the name of the implicit
parameter. 'param' and 'setParam' work like their unnamed counterparts
'param_' and 'setParam_', but they also take a proxy argument to specify the
name of the implicit parameter. The code above defines the wrappers @foo@ and
@bar@ and @setFoo@ and @setBar@ around @param@ and @setParam@ respectively,
which hide all the (slightly ugly) proxy stuff.

>>> putFooBar
foo was: ""
bar was: ""

Once again, the defaults of unspecified implicit parameters are given by the
'Default' class.

>>> setFoo "hello, world" putFooBar
foo was: "hello, world"
bar was: ""

>>> setBar "goodbye" $ setFoo "hello, world" putFooBar
foo was: "hello, world"
bar was: "goodbye"

An infix version of @setParam@ is also provided, '$$~'. Using @$$~@, the above
example would be:

>>> putFooBar $$~ (Proxy :: Proxy "foo", "hello, world") $$~ (Proxy :: Proxy "bar", "goodbye")
foo was: "hello, world"
bar was: "goodbye

-}

module Data.Implicit
    ( Implicit
    , param
    , setParam
    , ($$~)

    , Implicit_
    , param_
    , setParam_
    , ($~)
    )
where

import           Data.Default (Default, def)
import           GHC.TypeLits (Symbol)
import           GHC.Exts (Any)
import           Unsafe.Coerce (unsafeCoerce)


------------------------------------------------------------------------------
-- | The constraint @'Implicit' \"foo\" String@ on a function @f@ indicates
-- that an implicit parameter named @\"foo\"@ of type @String@ is passed to
-- @f@.
--
-- The name @\"foo\"@ is a type of kind 'Symbol' (from the "GHC.TypeLits"
-- module). The @DataKinds@ extension is required to refer to 'Symbol'-kinded
-- types.
class Implicit (s :: Symbol) a where
    _param :: proxy s -> a


------------------------------------------------------------------------------
instance Default a => Implicit s a where
    _param _ = def


------------------------------------------------------------------------------
-- | 'param' retrieves the implicit parameter named @s@ of type @a@ from the
-- context @'Implicit' s a@. The name @s@ is specified by a proxy argument
-- passed to @param@.
param :: Implicit s a => proxy s -> a
param = _param


------------------------------------------------------------------------------
-- | 'setParam' supplies a value for an implicit parameter named @s@ to a
-- function which takes a homotypic and homonymous implicit parameter. The
-- name @s@ is specified by a proxy argument passed to @setParam@.
setParam :: proxy s -> a -> (Implicit s a => b) -> b
setParam = using


------------------------------------------------------------------------------
-- | An infix version of 'setParam' with flipped arguments.
($$~) :: (Implicit s a => b) -> (proxy s, a) -> b
($$~) f (proxy, a) = using proxy a f


------------------------------------------------------------------------------
-- | The constraint @'Implicit_' String@ on a function @f@ indicates that an
-- unnamed implicit parameter of type @String@ is passed to @f@.
type Implicit_ = Implicit Any


------------------------------------------------------------------------------
-- | 'param_' retrieves the unnamed implicit parameter of type @a@ from the
-- context @'Implicit_' a@.
param_ :: Implicit_ a => a
param_ = param (Proxy :: Proxy Any)


------------------------------------------------------------------------------
-- | 'setParam_' supplies a value for an unnamed implicit parameter to a
-- function which takes a homotypic implicit parameter.
setParam_ :: a -> (Implicit_ a => b) -> b
setParam_ = setParam (Proxy :: Proxy Any)


------------------------------------------------------------------------------
-- | An infix version of 'setParam_' with flipped arguments.
($~) :: (Implicit_ a => b) -> a -> b
infixr 1 $~
f $~ a = using (Proxy :: Proxy Any) a f


------------------------------------------------------------------------------
data Proxy (s :: Symbol) = Proxy


------------------------------------------------------------------------------
newtype Lift a = Lift a


------------------------------------------------------------------------------
newtype Tagged (s :: Symbol) a = Tagged a


------------------------------------------------------------------------------
data Dict c where
    Dict :: c => Dict c


------------------------------------------------------------------------------
using :: proxy s -> a -> (Implicit s a => b) -> b
using p a = with (unlift (dict p a))
  where
    with :: Dict c -> (c => b) -> b
    with Dict b = b

    unlift :: Dict (c (Lift p)) -> Dict (c p)
    unlift = unsafeCoerce

    dict :: proxy s -> a -> Dict (Implicit s (Lift a))
    dict _ a' = let ?param = Tagged a' in Dict


------------------------------------------------------------------------------
instance (?param :: Tagged s a) => Implicit s (Lift a) where
    _param _ = let Tagged a = ?param in Lift a