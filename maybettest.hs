-- Test myself : https://hackage.haskell.org/package/MaybeT-0.1.2/docs/Control-Monad-Maybe.html

-- import System.Console.Readline
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

-- 'MaybeIO' is the type of computations which do IO, and which may fail.
type MaybeIO = MaybeT IO

readline :: String -> IO (Maybe String)
readline prompt = do
  putStrLn prompt
  l <- getLine
  return $ Just l

-- 'readline' already has type 'String -> IO (Maybe String)'; we just need
-- to wrap it.
maybeReadLine :: String -> MaybeIO String -- MaybeT IO (Maybe String)
maybeReadLine = MaybeT . readline

-- Fail if 'str' equals "quit".
failIfQuit :: String -> MaybeIO ()
failIfQuit str = if str == "quit"
                 then mzero
                 else return ()

-- This task may fail in several places.  Try typing Control-D or "quit" at
-- any prompt.
-- Take two lines as input and quit if "quit" is passed - use failIfQuit.
-- Print concatenation of two strings.
concatTwoInputs :: MaybeIO ()
concatTwoInputs = do
  s1 <- maybeReadLine "String 1>"
  failIfQuit s1
  s2 <- maybeReadLine "String 2>"
  failIfQuit s2
  lift $ putStrLn . concat $ [s1, s2]

-- Loop until failure.
main :: IO ()
main = do
  a <- runMaybeT concatTwoInputs
  case a of
    Nothing -> putStrLn "Bye bye"
    Just () -> main

-- when in doubt go to : https://wiki.haskell.org/All_About_Monads
