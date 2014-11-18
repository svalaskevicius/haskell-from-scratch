% Haskell from scratch
% Sarunas Valaskevicius
% November 22, 2014

# Functional programming 

## Differences from OO 

- *a style of building the structure and elements of computer programs, that treats computation as the evaluation of mathematical functions and avoids changing state and mutable data.* [Wikipedia]
- It is a *declarative programming paradigm*, which means programming is done with expressions
- *function* as first class citizen - can pass around, call from another context
- breaks data encapsulation, decouples for better *behaviour reusability*
    - data can still be bound by curying or closures

## You've used functional elements already
- map, reduce;
- promise pattern in javascript.. 

# Haskell. What is it? 

## Generic programming language

- desktop applications
- server side software


## Purity and referential transparency
- function with no side effects is pure
- this property is called referential transparency 
- it allows equational reasoning:

        y = f x
        g = h y y
        => 
        g = h (f x) (f x)

- *all* functions in haskell are pure 


## "No side effects"
- all data is passed immutably
- a function with same parameters will always return the same result
- reduces the risk of bugs - all changes to data are explicit


## Laziness
- most programming languages use eager evaluation 
- haskell is **lazy**: it will only compute a value when its actually used
- problematic in microcontroller space
- very convenient in generic programming - allows infinite computation definitions:

        printN n =  putStrLn . (intercalate " ") . (map show) . (take n)
        print10 = printN 10
        print10 [1..]
    
## Strong typing
- algebraic data types
- uses Hindley-Milner type inference
- *"If it type-checks, it's most likely good"*

        countSame :: Eq a => a -> [a] -> Int

    - `a` is a variable type
    - we only require to be able to compare two variables of type `a`
    - the same type a has to be passed to the 1st and 2nd params

## Pattern matching 
- powerful alternative for many `if` statements 
- functions are written in declarative manner
- destructuring matches

        maybePlus :: Int -> Maybe Int -> Maybe Int
        maybePlus a (Just t) = Just $ t + a
        maybePlus _ Nothing = Nothing

## Curried functions
- all functions return either the end result or function to get it:

        f :: a -> b -> c

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

# Hello world

----

    main :: IO()
    main = putStrLn "hello world" 

## "do" notation

    main :: IO()
    main = do
        putStrLn "hello"
        putStrLn "world"

Note: indentation matters.

# Type classes 

---

- similar to interfaces/abstract classes in OO
- can have default implementation

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

- choose which function to implement - `compare` or `(<=)`
- usage in functions

        sort :: Ord a => [a] -> [a] 


# Functional composition 

## Higher order functions 

  Maps and folds are very common in functional paradigm. 

## Functors 

## Applicative functors 

## Monoids
 
# Monads 
## Boxes and computation context
## Maybe monad 
## IO monad 


# Books to read

## learn you a haskell for great good 
## real world haskell 

# Extras

## monad transformers
## extensible effects 
## free monads
## category theory at the "magic" level 

