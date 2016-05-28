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
