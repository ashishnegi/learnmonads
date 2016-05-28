-- Learning Monads
-- Example of putting logging inside two functions : add/sub
add :: Int -> Int -> Int
add a b = a + b

sub :: Int -> Int -> Int
sub a b = a - b

-- We can compose add and sub.
tryval1 = (add 2 . sub 3 . add 5 $ 3) == -3 -- (2 + (3 - (5 + 3)))
