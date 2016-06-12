# learnmonads
My sample codes while learning Monads using the tutorial at :

1. http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
2. http://blog.sigfpe.com/2006/05/grok-haskell-monad-transformers.html
3. https://hackage.haskell.org/package/MaybeT-0.1.2/docs/Control-Monad-Maybe.html

```haskell
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
```
