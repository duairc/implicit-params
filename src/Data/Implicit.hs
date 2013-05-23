{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-}

module Data.Implicit
    ( Implicit
    , param
    , setParam
    , (~$)

    , Implicit_
    , param_
    , setParam_
    , ($~)
    )
where

import           Data.Default.Class (Default, def)
import           Unsafe.Coerce (unsafeCoerce)


------------------------------------------------------------------------------
-- | The constraint @'Implicit' \"foo\" String@ on a function @f@ indicates
-- that an implicit parameter named @\"foo\"@ of type @String@ is passed to
-- @f@.
--
-- The name @\"foo\"@ is a type of kind 'Symbol' (from the "GHC.TypeLits"
-- module). The @DataKinds@ extension is required to refer to 'Symbol'-kinded
-- types.
class Implicit s a where
    -- | 'param' retrieves the implicit parameter named @s@ of type @a@ from
    -- the context @'Implicit' s a@. The name @s@ is specified by a proxy
    -- argument passed to @param@.
    param :: proxy s -> a


------------------------------------------------------------------------------
instance Default a => Implicit s a where
    param _ = def


------------------------------------------------------------------------------
newtype Param s a b = Param (Implicit s a => b)


------------------------------------------------------------------------------
-- | 'setParam' supplies a value for an implicit parameter named @s@ to a
-- function which takes a homotypic and homonymous implicit parameter. The
-- name @s@ is specified by a proxy argument passed to @setParam@.
setParam :: forall a b proxy s. proxy s -> a -> (Implicit s a => b) -> b
setParam (_ :: proxy s) a f = unsafeCoerce (Param f :: Param s a b) (const a)
{-# INLINE setParam #-}


------------------------------------------------------------------------------
-- | An infix version of 'setParam' with flipped arguments.
(~$) :: (Implicit s a => b) -> proxy s -> a -> b
infixr 1 ~$
(~$) f proxy a = setParam proxy a f
{-# INLINE (~$) #-}


------------------------------------------------------------------------------
-- | The constraint @'Implicit_' String@ on a function @f@ indicates that an
-- unnamed implicit parameter of type @String@ is passed to @f@.
class Implicit_ a where
    param_ :: a


------------------------------------------------------------------------------
instance Default a => Implicit_ a where
    -- | 'param_' retrieves the unnamed implicit parameter of type @a@ from
    -- the context @'Implicit_' a@.
    param_ = def


------------------------------------------------------------------------------
newtype Param_ a b = Param_ (Implicit_ a => b)


------------------------------------------------------------------------------
-- | 'setParam_' supplies a value for an unnamed implicit parameter to a
-- function which takes a homotypic implicit parameter.
setParam_ :: forall a b. a -> (Implicit_ a => b) -> b
setParam_ a f = unsafeCoerce (Param_ f :: Param_ a b) a
{-# INLINE setParam_ #-}


------------------------------------------------------------------------------
-- | An infix version of 'setParam_' with flipped arguments.
($~) :: (Implicit_ a => b) -> a -> b
infixr 1 $~
f $~ a = setParam_ a f
{-# INLINE ($~) #-}
