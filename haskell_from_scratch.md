# What is functional programming 

- how is it different from OO 
    * function as first class citizen --  can pass around, call from another context 
    * breaks data encapsulation, decouples for better behaviour reusability
    * data can still be bound by curying or closures 

- you've used functional elements already:
    * map / reduce;
    * promise pattern in javascript.. 

# Haskell. What is it? 
- purity and referential transparency
    + function with no side effects is pure
    + this property is called referential transparency 
    + it allows equational reasoning:

            y = f x
            g = h y y
               =>
            g = h (f x) (f x)

    + all* functions in haskell are pure 
- laziness
    * most of the languages use eager evaluation 
    * haskell is lazy: it will only compute a value when its actually used
    * problematic in microcontroller space
    * very convenient in generic programming - allows infinite computation definitions
- strong typing
    + uses Hindley-Milner type inference
    + "If it type-checks, it's most likely good" 
- pattern matching 
    + powerful alternative for many `if` statements 
    + functions are written in declarative manner
- curried functions
    + all functions return either the end result or function to get it:

        f :: a -> b -> c

        `f 3` has type `b -> c`, which is a function itself. 

- immutable data 
    + all data is passed immutably
    + reduces the risk of bugs
- asynchronous
    + haskell uses green threads 
    + events based execution 
    + all that is abstracted underneath the language 
    + green threads can be executed by any amount of OS threads, maximum specified at compile time 
- modules
    + export closely related functions 
    + hide private implementation 

# So what can I use it for? 
Generic programming - applications, server side software, even Web pages when compiled to js.. 

 *Microcontroller projects in haskell usually generate eager code instead.

# Ecosystem
- compiler choices 
    + ghc is the current preference, has multiple backends (native, llvm, c) 
    + jhc performs whole program optimisation 
    + ... 
- ghci (repl)
    + check quickly before coding
- packaging 
    - cabal - dependency manager and more 
    - hackage - repository for haskell packages 
    - hoogle - function search engine 
- tdd tools 
    - quickcheck - randomised testing framework for predefined properties 
    - hspec - tdd support tool

# Hello world
- IO monad at the "magic"/usage level - defines recipes for impure computation while still being pure

        main :: IO()
        main = putStrLn "hello world"

- syntactic sugar: `do` notation 

        main :: IO()
        main = do
            putStrLn "hello"
            putStrLn "world"

# Type classes 
- similar to interfaces in OO
- can have default implementation
- uses in functions

        sort :: Ord a => [a] -> [a] 


# Functional composition 
- higher order functions 

     Maps and folds are very common in functional paradigm. 

- point free style 
    * Points are arguments to functions;
    * point free style tells to combine functions while avoiding explicit argument usage 

            timesTwo = map (*2)

- functors 
- applicative functors 
- monoids
 
# Monads 
- boxes and computation context
- maybe monad 
- io monad 


# Books to read
- learn you a haskell for great good 
- real world haskell 

---

# Extras
- monad transformers
- extensible effects 
- free monads
- category theory at the "magic" level 


---

All done while developing a single app - metrics api for Inviqa 
