import System.Random

-- Learning Monads
-- Example of putting logging inside two functions : add/sub
add :: Int -> Int -> Int
add a b = a + b

sub :: Int -> Int -> Int
sub a b = a - b

-- We can compose add and sub.
tryval1 :: Bool
tryval1 = (add 2 . sub 3 . add 5 $ 3) == -3 -- (2 + (3 - (5 + 3)))

-- Adding loggin in add and sub
add' :: Int -> Int -> (Int, String)
add' a b = (add a b, show a ++ " + " ++ show b ++ " = " ++ show (add a b))

sub' :: Int -> Int -> (Int, String)
sub' a b = (sub a b, show a ++ " - " ++ show b ++ " = " ++ show (sub a b))

-- Adding a bind function to compose add' and sub'
bind' :: (Int -> (Int, String)) -> (Int -> (Int, String)) -> (Int -> (Int, String))
bind' f g = \x -> let (y, s1) = g x
                      (z, s2) = f y
                  in (z, s1 ++ " =>> " ++ s2)

-- Still able to compose add' and sub'
tryval2 :: Bool
tryval2 = let f = bind' (add' 2) $ bind' (sub' 3) (add' 5)
              (v, _) = f 3
          in v == -3

-- Writing my own Logging data.
data Logging a = Logging (a, String) deriving (Show)

instance Functor (Logging) where
  fmap f (Logging (a, s)) = Logging (f a, s)

instance Applicative (Logging) where
  pure a = Logging (a, "")
  Logging (f, s1) <*> Logging (a, s2) = Logging (f a, s1 ++ s2)

-- Here is the monad.
instance Monad (Logging) where
  Logging (a, s1) >>= f = let Logging (b,s2) = f a
                          in Logging (b, s1 ++ s2)

-- Same like add' and sub'
add'' :: Int -> Int -> Logging Int
add'' a b = Logging (add a b, show a ++ " + " ++ show b ++ " = " ++ show (add a b) ++ ";")

sub'' :: Int -> Int -> Logging Int
sub'' a b = Logging (sub a b, show a ++ " - " ++ show b ++ " = " ++ show (sub a b) ++ ";")

-- Composing using bind (>>=): 2 + (3 - (5 + 3)) but read code as piping value.
tryval3 :: Logging Bool
tryval3 = fmap (== -3) $ add'' 5 3 >>= sub'' 3 >>= add'' 2

--------------------- List Monad -------------------------
----------------------------------------------------------
halfs :: Float -> [Float]
halfs a = [0, a/2, a]

thirds :: Float -> [Float]
thirds a = [0, a/3, 2*a/3, a]

bind'' :: (Float -> [Float]) -> [Float] -> [Float]
bind'' f vs = concat $ map f vs

halfthirds :: Float -> [Float]
halfthirds x = bind'' halfs . thirds $ x

halfthirdsMonadic :: Float -> [Float]
halfthirdsMonadic x = [z | y <- thirds x, z <- halfs y]

------------------- Random Monad -----------------------
--------------------------------------------------------
incRandom :: Int -> StdGen -> (Int, StdGen)
incRandom v g = let (a, g') = random g
                in (v + a, g')

strRandom :: Int -> StdGen -> (String, StdGen)
strRandom v g = let (a, g') = random g :: (Int, StdGen)
                in (show v ++ ":" ++ show a, g')

bindRandom :: (a -> StdGen -> (b, StdGen)) -> (StdGen -> (a, StdGen)) -> (StdGen -> (b, StdGen))
bindRandom f g = \sg -> let (a, g') = g sg
                        in f a g'

bindFGtest :: Int -> StdGen -> (String, StdGen)
bindFGtest = bindRandom strRandom . incRandom  -- want to do .. (strRandom (incRandom 1)

tryRandom :: Int -> Int -> (String, StdGen)
tryRandom initial seed = let g = mkStdGen seed
                         in bindFGtest initial g


-- Some thoughts from the tutorial :  http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
-- Monads

-- It's now time to step back and discern the common structure.

-- Define

-- type Debuggable a = (a,String)
-- type Multivalued a = [a]
-- type Randomised a = StdGen → (a,StdGen)


-- Use the variable m to represent Debuggable, Multivalued or Randomised.
-- In each case we are faced with the same problem. We're given a function a -> m b
-- but we need to somehow apply this function to an object of type m a instead of one of type a.
-- In each case we do so by defining a function called bind of type (a → m b) -> (m a → m b) and
-- introducing a kind of identity function unit :: a → m a. In addition, we found that these identities held:
--    f * unit = unit * f = f and
--    lift f * lift g = lift (f.g),
-- where '*' and lift where defined in terms of unit and bind.

-- So now I can reveal what a monad is. The triple of objects (m,unit,bind) is the monad,
-- and to be a monad they must satisfy a bunch of laws such as the ones you've been proving.
-- And I think that in each of the three cases you'd have eventually come up with a function like bind,
-- even if you might not have immediately noticed that all three cases shared a common structure.


-- Input/Output

-- There's now one last thing we have to look at before you're fully qualified in monadicity.
-- Interaction with the outside world. Up until now everything I have said might apply to any pure functional language.
-- But now consider lazy pure functional languages. In such a language you have no idea what order things will be evaluated in.
-- So if you have a function to print the message "Input a number" and another function to input the number,
-- you might not be able to guarantee that the message is printed before the input is requested!
-- Go back to the randomised function example. Notice how the random seed gets threaded through your functions
-- so that it can be used each time random is called. There is a kind of ordering going on.
-- Suppose we have x >>= f >>= g. Because g uses the seed returned by f, we know for sure
-- that f will generate its random number before g. This shows that in principle, monads can be used to order computations.
