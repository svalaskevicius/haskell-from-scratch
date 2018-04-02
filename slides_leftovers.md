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
