{-# LANGUAGE EmptyDataDecls, DataKinds, FlexibleContexts, RankNTypes #-}
import Data.Implicit
import Data.Proxy

data Foo
data Bar

foo :: Proxy Foo
foo = Proxy

bar :: Proxy Bar
bar = Proxy

putFooBar :: (Implicit Foo String, Implicit Bar String) => IO ()
putFooBar = do
    putStrLn $ "foo was: " ++ show (param foo :: String)
    putStrLn $ "bar was: " ++ show (param bar :: String)
