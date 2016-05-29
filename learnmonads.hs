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

--------------------- Next Monad -------------------------
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
