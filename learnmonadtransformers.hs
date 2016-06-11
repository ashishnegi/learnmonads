module LearnMonadTransformers where

import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative
import Data.Char

-- http://blog.sigfpe.com/2006/05/grok-haskell-monad-transformers.html
test1 = do
            a <- get
            modify (+1)
            b <- get
            return (a,b)

test2 = do
            a <- get
            modify (++"1")
            b <- get
            return (a,b)

go1 = evalState test1 0
go2 = evalState test2 "0"

test3 :: StateT Int (StateT [Char] Identity) (Int, [Char])
test3 = do
    modify (+ 1)
    lift $ modify (++ "1")
    a <- get
    b <- lift get
    return (a,b)

go3 = runIdentity $ evalStateT (evalStateT test3 0) "0"

test5 :: StateT Int IO ()
test5 = do
    modify (+ 1)
    a <- get
    lift (print a)
    modify (+ 1)
    b <- get
    lift (print b)

-- go5 = evalStateT test5 0

test7 :: StateT Int (State String) (Int, String)
test7 = do
    modify (+ 1)
    lift $ modify (++ "1")
    a <- get
    b <- lift get
    return (a,b)

go7 = evalState (evalStateT test3 0) "0"


-- https://en.wikibooks.org/wiki/Haskell/Monad_transformers

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f a = MaybeT $ fmap (\x -> fmap f x) (runMaybeT a)

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  mf <*> ma = MaybeT $ (\f a -> f <*> a) <$> (runMaybeT mf) <*> (runMaybeT ma)

instance (Monad m) => Monad (MaybeT m) where
  mma >>= f = MaybeT $ do
      ma <- runMaybeT mma -- Maybe a
      case ma of
        Nothing -> return Nothing
        Just a -> runMaybeT $ f a

instance (Alternative m) => Alternative (MaybeT m) where
  empty = MaybeT $ pure Nothing
  mma <|> mmb = MaybeT $ (runMaybeT mma) <|> (runMaybeT mmb)

instance (Alternative m, Monad m) => MonadPlus (MaybeT m)

-- instance (Alternative m, Monad m) => MonadPlus (MaybeT m) where
  -- mzero = empty
  -- mplus = (<|>)

 -- mzero = MaybeT $ pure Nothing
 -- mplus mmma mmmb = MaybeT $ do
 --   ma <- runMaybeT mmma
 --   case ma of
 --     Nothing -> runMaybeT mmmb
 --     Just a -> return ma

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

getPassphrase :: MaybeT IO String
getPassphrase = do
  password <- lift getLine
  guard $ isValid password
  return password

askPassphrase :: MaybeT IO ()
askPassphrase = do
  lift $ putStrLn "Enter password < 8 , alpha, number and punctuation:"
  p <- getPassphrase
  lift $ putStrLn $ "Your password is " ++ p

-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

main :: IO ()
main = do
  a <- runMaybeT askPassphrase
  return ()
