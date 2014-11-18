% Haskell from scratch
% Sarunas Valaskevicius
% November 22, 2014

# Functional programming 

## Differences from OO 

- *function* as first class citizen - can pass around, call from another context
- breaks data encapsulation, decouples for better *behaviour reusability*
- data can still be bound by curying or closures

## You've used functional elements already
- map, reduce;
- promise pattern in javascript.. 

# Haskell. What is it? 

## Purity and referential transparency
- function with no side effects is pure
- this property is called referential transparency 
- it allows equational reasoning:

        y = f x
        g = h y y
        => 
        g = h (f x) (f x)

- *all* functions in haskell are pure 

## Laziness
- most of the languages use eager evaluation 
- haskell is lazy: it will only compute a value when its actually used
- problematic in microcontroller space
- very convenient in generic programming - allows infinite computation definitions
    
## Strong typing
- uses Hindley-Milner type inference
- *"If it type-checks, it's most likely good"*

## Pattern matching 
- powerful alternative for many `if` statements 
- functions are written in declarative manner

## Curried functions
- all functions return either the end result or function to get it:

        f :: a -> b -> c

- `f 3` has type `b -> c`, which is a function itself. 

## Immutable data 
- all data is passed immutably
- reduces the risk of bugs

## Asynchronous
- haskell uses green threads 
- events based execution 
- all that is abstracted underneath the language 
- green threads can be executed by any amount of OS threads, maximum specified at compile time 

## Modules
- export closely related functions 
- hide private implementation 


## So what can I use it for? 

Generic programming - applications, server side software, even web pages when compiled to js.. 

(Microcontroller projects in haskell usually generate eager code instead)



# Ecosystem

## Compiler choices 

- ghc is the current preference, has multiple backends (native, llvm, c) 
- jhc performs whole program optimisation 
- ... 

## ghci (repl)

- check quickly before coding

## Packaging 

- cabal - dependency manager and more 
- hackage - repository for haskell packages 
- hoogle - function search engine 

## TDD tools 

- quickcheck - randomised testing framework for predefined properties 
- hspec - tdd support tool

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

