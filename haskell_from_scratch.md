% Haskell from scratch
% Sarunas Valaskevicius
% November 22, 2014

# Functional programming 

## What is it?

- *a style of building the structure and elements of computer programs, that treats computation as the evaluation of mathematical functions and avoids changing state and mutable data.* [Wikipedia]
- It is a *declarative programming paradigm*, which means programming is done with expressions
- *function* as first class citizen
- breaks data encapsulation as perceived in OO, decouples for better *behaviour reusability*
    - data can still be bound by curying or closures

<div class="notes">
- focuses on behaviour
- declare the functional relation, rather than "tell computer how to do stuff"
- can functions pass around, call from another context, even return them
</div>

## You've used functional elements already
- map, reduce;
- closures;
- promise pattern in javascript.. 

# About haskell

## Generic programming language

- desktop applications
- server side software
- known to be especially good at DSLs

<div class="notes">
Many examples for *desktop* include:
- xmonad - the famous window manager
- pandoc - document converter

Serverside:
- yesod - web framework
- many companies include *some* haskell based component in their stack
</div>

## Purity and referential transparency
- function with no side effects is pure
- this property is called referential transparency 
- it allows equational reasoning:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
y = f x
g = h y y
    => 
g = h (f x) (f x)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- *all* functions in haskell are pure 


## "No side effects"
- all data is passed immutably
- a function with same parameters will always return the same result
- reduces the risk of bugs - all changes to data are explicit


## Laziness
- most programming languages use eager evaluation 
- but some start to implement lazy *generators*
- haskell is **lazy**: it will only compute a value when its actually used
- problematic in microcontroller space
- very convenient in generic programming - allows infinite computation definitions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
printN n =  putStrLn . (intercalate " ") . (map show) . (take n)

print10 = printN 10

print10 [1..]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
## Strong typing
- algebraic data types
- uses Hindley-Milner type inference
- *"If it type-checks, it's most likely good"*

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
    countSame :: Eq a => a -> [a] -> Int
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    - `a` is a variable type
    - we only require to be able to compare two variables of type `a`
    - the same type a has to be passed to the 1st and 2nd params

## Pattern matching 
- powerful alternative for many `if` statements 
- functions are written in declarative manner
- destructuring matches

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
    maybePlus :: Int -> Maybe Int -> Maybe Int
    maybePlus a (Just t) = Just $ t + a
    maybePlus _ Nothing = Nothing
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Curried functions
- all functions return either the end result or function to get it:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
    f :: a -> b -> c
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `f 3` has type `(b -> c)`, which is a function itself. 

## Asynchronous
- GHC uses green threads (implemented in the VM)
    - events based execution - doesn't block the process
    - all that is abstracted underneath the language 
- green threads can be executed by any amount of OS threads, specified at compile time 

## Modules
- export closely related functions 
- hide private implementation 
- conceptually similar to module pattern in javascript

        Data.List

- directory path is mapped to the module name



# Ecosystem

## Compiler choices 

- **ghc** is the current preference, has multiple backends (native, llvm, c) 
- **jhc** performs whole program optimisation 
- ... 

## ghci (repl)

- check quickly before coding

## Packaging 

- **cabal** - dependency manager and more 
- **hackage** - repository for haskell packages 
- **hoogle** - function search engine 
- **hayoo** - another search engine

## TDD tools 

- **quickcheck** - randomised testing framework for predefined properties 
- **hspec** - tdd support tool

# Generic language constructs

## Defining data types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
data MyType = MyIntType Int | MyEmptyType | MyStringType String
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defines a new type `MyType` and provides three data constructors.

## Type alias

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
type Deck = [Card]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defines a type alias. The data can still be accessed using the original type.

## Newtype 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
newtype Deck = Deck [Card]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A combination of `data` and `type` - the usage of the resulting type is
that of a `data` type, however the runtime is of a type alias.

## Function declaration


~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
myFunction :: Type
myFunction arguments = body
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defines a new function available within the module.

## Let .. in

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
myFunction :: Int
myFunction = let x = 1
             in x + 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Where

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
myFunction :: Int
myFunction = increasedX
    where x = 1
          increasedX = x + 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Do notation

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
myFunction :: IO Int
myFunction = do
    otherFunction
    return 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Let inside do

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
myFunction :: IO Int
myFunction = do
    let x = 1
    return x
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hello world

----

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
main :: IO()
main = putStrLn "hello world" 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## The "do" notation

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
worldType :: IO String
worldType = return "haskell"

main :: IO()
main = do
    whatWorld <- worldType
    putStrLn $ "hello " ++ whatWorld ++ " world"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note: indentation matters.

# Type system 

## What's a type class?

- similar to interfaces/abstract classes in OO
- can have default implementation

---

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- choose which function to implement - `compare` or `(<=)`

## Deriving a type class

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
data MyType = MyType Int deriving Eq

instance Ord MyType where
    (MyType a) <= (MyType b) = a <= b
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*(in this case, simply `deriving (Eq, Ord)` would also have worked)*

## Usage in functions

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
findLowerThan :: Ord a => a -> [a] -> [a] 
findLowerThan measure = filter (< measure)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elements of functional programming

## Composing two functions

- In haskell, it is possible to compose two functions to one using the (.) operator:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
    foo :: Int -> String
    foo = show

    bar :: String -> [String]
    bar x = [x]

    foobar = bar . foo

    foobar 5  -- ["5"]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- its not a language construct - its a function, defined in the `Prelude`.

## Monoids

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
class Monoid a where
        mempty  :: a
        -- ^ Identity of 'mappend'
        mappend :: a -> a -> a
        -- ^ An associative operation
        mconcat :: [a] -> a
        mconcat = foldr mappend mempty
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Monoid laws

- Identity law
    - `mappend mempty x` = `x`
    - `mappend x mempty` = `x`
- Associative law
    - `mappend x (mappend y z)` = `mappend (mappend x y) z`

## Monoid example

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
Sum 4 <> Sum 3 <> Sum 2 <> Sum 1
--    Sum{getSum = 10}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
mconcat . (map Sum) $ [1..4]
--    Sum{getSum = 10}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
mconcat . (map $ Just . Sum) $ [1..4]
--    Just (Sum{getSum = 10})
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
Just (Sum 10) <> Nothing <> Just (Sum 5)
--    Just (Sum{getSum = 15})
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Higher order functions 

Wikipedia: *"In mathematics and computer science, a higher-order function is a function that does at least one of the following:*

- *takes one or more functions as an input*
- *outputs a function"*

For example, map and fold (reduce) are very common in functional paradigm. 

## Boxes and computation context

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
data MyType a = MyType { usedValue :: a }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- a box, as an analogy, is a useful explanation of a parametrised type
- `MyType` is a box for any type `a`

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
myFunction :: f a -> f a
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `f` is a variable "box" where we don't specify its type
- the only property specified in the function definition is that the type has to have a type parameter

## Functors 

- functor allows mapping over them
- definition:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
    class Functor f where
      fmap :: (a -> b) -> f a -> f b
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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

## Example

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
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

 
## Applicative functors 

- applicative functor is a functor accepting wrapped functions
- it's definition:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
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

## Applicative functor example
 
- apply a "boxed" function to a "boxed" value:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
    Just (+1) <*> Just 1                 -- Just 2
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- apply a binary function:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
    Just (+) <*> Just 1 <*> Just 4       -- Just 5
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
    Just (+) <*> Just 1 <*> Nothing      -- Nothing
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- a shorthand for fmap:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
    (+) <$> Just 1 <*> Just 4            -- Just 5
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Monads 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
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

## Monad laws

- identity:
    - `return a >>= k`  =  `k a`
    - `m >>= return`  =  `m`
- associativity:
    - `m >>= (\x -> k x >>= h)`  =  `(m >>= k) >>= h`

## Maybe monad 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
Just 1 >>= (\a -> return $ a+1)  -- or just "Just 1 >>= return . (+1)"
-- Just 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `do` notation is just syntactic sugar over `>>=`!

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
argument :: Maybe Int
argument = return 1

computation :: Maybe Int
computation = do
    a <- argument
    return $ a + 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## IO monad 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
echo :: IO ()
echo = getLine >>= putStrLn
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extras

## Books to read

- learn you a haskell for great good 
- real world haskell 
- ...

## lenses
## monad transformers
## extensible effects 
## free monads
## category theory

