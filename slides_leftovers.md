name: inverse
layout: true
class: inverse

---
class: middle
# Introduction to Functional Programming

---

# Generic language constructs


---
---
## Type alias

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
type Deck = [Card]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defines a type alias. The data can still be accessed using the original type.

---
## Newtype

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
newtype Deck = Deck [Card]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A combination of `data` and `type` - the usage of the resulting type is
that of a `data` type, however the runtime is of a type alias.



---
## Guards!

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
alg :: [Integer] -> Integer -> Integer
alg (x:x':xs) a
 | x == a = x' + alg xs x'
 | otherwise = alg (x':xs) x
alg [_] _ = 0
alg [] _ = 0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Allows computation in the matching.

---
## Case .. of

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
alg :: [Integer] -> Integer -> Integer
alg (x:x':xs) a = case x == a of
                    True -> x' + alg xs x'
                    _ -> alg (x':xs) x')
alg [_] _ = 0
alg [] _ = 0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pattern-matches in the code.

---
## If

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
alg :: [Integer] -> Integer -> Integer
alg (x:x':xs) a = if x == a then x' + alg xs x'
                  else alg (x':xs) x'
alg [_] _ = 0
alg [] _ = 0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## Let .. in

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
myFunction :: Int
myFunction = let x = 1
             in x + 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## Where

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
myFunction :: Int
myFunction = increasedX
    where x = 1
          increasedX = x + 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## Do notation

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
myFunction :: IO Int
myFunction = do
    other <- otherFunction
    return $ 1 + other
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## Let inside do

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
myFunction :: IO Int
myFunction = do
    let x = 1
    return x
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
# Hello world


~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
main :: IO()
main = putStrLn "hello world"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## The "do" notation

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
worldType :: IO String
worldType = return "Haskell"

main :: IO()
main = do
    whatWorld <- worldType
    putStrLn $ "hello " ++ whatWorld ++ " world"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note: indentation matters.

---
# Type system

---
# Elements of functional programming

---
## Boxes and computation context

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
data MyType a = MyType { usedValue :: a }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- a box, as an analogy, is a useful explanation of a parametrised type
- `MyType` is a box for any type `a`

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
myFunction :: f a -> f a
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `f` is a variable "box" where we don't specify its type
- the only property specified in the function definition is that the type has to have a type parameter





---
## Monoids

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
class Monoid a where
        mempty  :: a
        -- ^ Identity of 'mappend'
        mappend :: a -> a -> a
        -- ^ An associative operation
        mconcat :: [a] -> a
        mconcat = foldr mappend mempty
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## Monoid laws

- Identity law
    - `mappend mempty x` = `x`
    - `mappend x mempty` = `x`
- Associative law
    - `mappend x (mappend y z)` = `mappend (mappend x y) z`

---
## Monoid example

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
Sum 4 <> Sum 3 <> Sum 2 <> Sum 1
--    Sum{getSum = 10}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
mconcat . (map Sum) $ [1..4]
--    Sum{getSum = 10}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
mconcat . (map $ Just . Sum) $ [1..4]
--    Just (Sum{getSum = 10})
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
Just (Sum 10) <> Nothing <> Just (Sum 5)
--    Just (Sum{getSum = 15})
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


---
## Functors

- functor allows mapping over them
- definition:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
    class Functor f where
      fmap :: (a -> b) -> f a -> f b
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


---
## Functor laws

- identity:
    - `fmap id` = `id`
- composition:
    - `fmap (p . q)` = `(fmap p) . (fmap q)`

<div class="notes">
- `f` is the computation context
- fmap takes a function, and a value in the context
- then fmap applies the function to an unwrapped value
- and returns wrapped result
</div>

---
## Example

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
numbers = [1..10]
strings = fmap show numbers
-- ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

notSureIfNumber = Just 9
notSureIfString = fmap show notSureIfNumber
-- Just "9"

notSureIfNumber = Nothing
notSureIfString = fmap show notSureIfNumber
-- Nothing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


---
## Applicative functors

- applicative functor is a functor accepting wrapped functions
- it's definition:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
    class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
        (*>) :: f a -> f b -> f b
        (<*) :: f a -> f b -> f a
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<div class="notes">
- same as functor, however allows the function to be wrapped too!
- unwraps the function and the argument
- "unwrapping" means to execute the context rules, take the value
- applies the function
- wraps the result back
</div>

---
## Applicative functor example

- apply a "boxed" function to a "boxed" value:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
    Just (+1) <*> Just 1                 -- Just 2
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- apply a binary function:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
    Just (+) <*> Just 1 <*> Just 4       -- Just 5
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
    Just (+) <*> Just 1 <*> Nothing      -- Nothing
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- a shorthand for fmap:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
    (+) <$> Just 1 <*> Just 4            -- Just 5
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


---
## Monads

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
class  Monad m  where
    return      :: a -> m a
    (>>=)       :: m a -> (a -> m b) -> m b
    (>>)        :: m a -> m b -> m b

    m >> k      = m >>= \_ -> k
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

<div class="notes">
- `return` - Inject a value into the monadic type.
- `(>>=)` - Sequentially compose two actions, passing any value produced by the first as an argument to the second.
- `(>>)` - Sequentially compose two actions, discarding any value produced by the first, like sequencing operators (such as the semicolon) in imperative languages.
</div>

---
## Monad laws

- identity:
    - `return a >>= k`  =  `k a`
    - `m >>= return`  =  `m`
- associativity:
    - `m >>= (\x -> k x >>= h)`  =  `(m >>= k) >>= h`

---
## Maybe monad

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
Just 1 >>= (\a -> return $ a+1)  -- or just "Just 1 >>= return . (+1)"
-- Just 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `do` notation is just syntactic sugar over `>>=`!

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
computation :: Maybe Int
computation = do
    a <- Just 1
    return $ a + 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## IO monad

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.Haskell}
echo :: IO ()
echo = getLine >>= putStrLn
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
# Summary

---
## You've seen

- overview of Haskell ecosystem
- basic language constructs
- functional programming constructs

---
## Where to next?

- code!
- learn you a Haskell for great good
- real world Haskell
- /r/Haskell
- code more!

---
## Thanks

Any questions?
