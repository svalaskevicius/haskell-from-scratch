name: inverse
layout: true
class: inverse

---
class: middle
# Introduction to Functional Programming

---
## What to expect from this course

- an overview of Functional Programming and basic concepts
- basic Haskell syntax (yes, we'll use Haskell!)
- being able to build a program!

### Why Haskell?

- It's purely functional, so it's easier to identify the concepts being explained.
- Paradigm concepts are easily translated to other languages, such as Scala
- Because it's awesome
- It does not allow you to shoot yourself in the foot
- Because it's elegant and clean, it becomes easy to read once you know the
  minimal syntax

---

# Functional programming

Simply put, it is programming with functions.

A function is:

- a description of a computation (a mapping from its input values to an output value);
- a value that can be passed over (or be returned from) to another function.

```haskell
myFunction :: TypeOfArg1 -> TypeOfArg2 -> ... -> ReturnType
myFunction arg1 arg2 ... = body
```

We can call the function like this:

```haskell
myFunction arg1 arg2
```

Functions can also be *pure* and *total*.

---
## Pure functions
A function where the return value can only be affected by its input paramaters, and it does not
produce any observable side-effect is called a *pure* function - the only effect of the function is
the produced return value.

- this property is called *referential transparency*
- it allows *equational reasoning*:

```haskell
y = f x
g y == g (f x)
```

Or, if we know that `f x` is 42, we could simply replace `g y` with `g 42` and be sure that the
program still works exactly the same.

---
## Total functions
A function is called *total* when it has a return value for every combination of its possible
input values.

For example, a total function could look like this:
```haskell
f :: Integer -> Integer
f x = 2*x
```

Another example, where the function is only defined for just some input values, is **not** a total function:

```haskell
f :: Integer -> Integer -> Integer
f x y = x `div` y
```

```
> f 4 2
2
> f 4 0
*** Exception: divide by zero
```

---
## Defining data types

```haskell
data MyType = MyIntType Int | MyEmptyType | MyStringType String
```

Defines a new type `MyType` and provides three alternative data constructors:
 - `MyIntType` has one Integer parameter
 - `MyEmptyType` has no parameters
 - `MyStringType` has one String parameter


```haskell
> let x = MyIntType 1
> :type x
x :: MyType
```

---
## List

A data structure that keeps a list of elements of the same type.

```haskell
data List a = Nil | Cons a (List a)
```

A list can either be empty list `Nil`, or `Cons`, that prepends an element of type `a` to a list of the same type.

A list of type `a` in Haskell is denoted as `[a]`. `Nil` is represented as `[]` and `Cons` - `:`.

```haskell
> [1, 2, 3]
[1,2,3]
> 1 : 2: 3 : []
[1,2,3]
```

Some common functions:
 - `head [1, 2, 3] = 1`
 - `tail [1, 2, 3] = [2, 3]`
 - `take 2 [1, 2, 3] = [1, 2]`

---

## Ranges

A simple way to create lists of number ranges, is to use the `..` operator.
For example:

```haskell
> [1..5]
[1,2,3,4,5]
```


We can specify the increment amount for a range by telling the compiler what
the second elment of the sequence will be:

```haskell
> [1,3..11]
[1,3,5,7,9,11]
```

We can also create infinite lists of numbers by not specifying the end limit
value for the range, as so:

```haskell
[1..]
```
Because Haskell is lazy, you can create such lists and the compiler will be
able to handle them.

Ranges can be used to create lists of characters:

```haskell
> ['a'..'f']
"abcdef"
> ['a','c'..'z']
"acegikmoqsuwy"
```

---

# Going further

Let's look as some more advanced features provided by Haskell:

 - Pattern matching
 - Strong type system
 - Curried functions
 - Lambda functions
 - Composing two functions
 - Higher order functions
 - Lazy evaluation

---
## Pattern matching
- powerful alternative for many `if` statements
- functions are written in declarative manner
- destructuring matches

```haskell
maybeDiv :: Int -> Int -> Maybe Int
maybeDiv _ 0 = Nothing
maybeDiv a b = Just (a `div` b)
```

If the second parameter is `0`, the function will match the first case - `maybeDiv _ 0` and
return `Nothing` as the result - parameters marked with `_` match everything and are ignored.
For all other cases, the second line will match as the default case:
`maybeDiv a b` and capture the argument values as `a` and `b`.

```haskell
maybeDiv 10 2 == Just 5 -- True
maybeDiv 10 0 == Nothing -- True
```
---
### Pattern matching (2)

Let's create a Fibonacci number generator!

```haskell
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

 - First the input argument is matched against the literal `0` - if it matches the function returns `1` (`fib 0 = 1`).
 - Otherwise, it is matched against the literal number `1` - if it matches the function returns `1` (`fib 1 = 1`).
 - If the above checks don't match, the input argument is matched against a variable `n`, which will match *any value* that was passed to the function, and place it in the variable `n`. At this point, the variable `n` can be used in the expression on the right side to make recursive calls to `fib` (`fib n = fib (n-1) + fib (n-2)`).

---
### Destructuring in pattern matching

```haskell
maybePlus :: Int -> Maybe Int -> Maybe Int
maybePlus a (Just b) = Just (b + a)
maybePlus _ Nothing = Nothing
```

If the second parameter is a value of `Just t` - a possible value of `Maybe Int` type, it matches
the first case and labels the integer contained in the value as `t`. Alternatively, the second case
matching `maybePlus _ Nothing` will capture all inputs where the second argument is `Nothing`.

```haskell
maybePlus 10 (Just 5) == Just 15 -- True
maybePlus 10 Nothing == Nothing -- True
```

or, combined with the function defined earlier:

```haskell
maybePlus 10 (maybeDiv 10 2) == Just 15 -- True
maybePlus 10 (maybeDiv 10 0) == Nothing -- True
```

---
## Strong typing

- *"If it type-checks, it's most likely good"*
- types not only help to ensure that a program works correctly, but also provide a headline of
what a function is doing
- for convenience, most compilers implement type inference

```haskell
maybePlus :: Int -> Maybe Int -> Maybe Int

...

maybePlus (maybeDiv 10 0) 10
```

```
<interactive>:58:12: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Maybe Int’
    • In the first argument of ‘maybePlus’, namely ‘(maybeDiv 10 0)’
      In the expression: maybePlus (maybeDiv 10 0) 10
      In an equation for ‘it’: it = maybePlus (maybeDiv 10 0) 10
```

---
## Strong typing (2)

```haskell
map :: (a -> b) -> [a] -> [b]
```

Given the above function signature, we can already say what the function is doing:
 - given a function `(a -> b)`
 - and a list `[a]`
 - it will map over the list, applying the given function and return a list of its return values

Note: the example here uses variable types, and Haskell will infer the actual types based on the
usage of the function - as long as the types marked with the same letter (a or b) are the same, the
function will work correctly.

---
## Curried functions

All functions in Haskell either return the end result or a function to get it:

```haskell
f :: a -> b -> c
```

`f 3` has type `(b -> c)`, which is a function itself. An example of this can be a sum:

```haskell
addNumbers x y = x + y
addFive = addNumbers 5
addFive 2 -- will return 7
```

`addNumbers 5` returns a function that takes one parameter `y` and results in `5 + y`.

In Haskell, all functions are curried.
---

## Lambda functions

It is frequently convenient to express simple functions anonymously - without assigning a specific
label to them, but rather just "embedding" them in the function call directly - as seen in the example
below, where 5 is being added to every element of the list.

The syntax to declare a lambda function is: `\parameters -> body`.

```haskell
map (\x -> x+5) [1..]
```

Why `\`? Because `(\` resembles a lambda symbol (if you squint hard enough).

---
## Composing two functions

In haskell, it is possible to compose two functions to one using the `.` operator:

```haskell
foo :: Int -> String
foo = show

bar :: String -> [String]
bar x = [x]

foobar = bar . foo

foobar 5  -- ["5"]
```

```haskell
> :info foobar
foobar :: Int -> [String]
```

`bar` composed with `foo` will call the function `foo` first, and pass its results to the argument of `bar` returning the final results.

---
## Composing two functions (2)

The `.` operator is not a specific language construct - like many others, it is a simple function, defined in the *Prelude*:

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f (g x)
```

It takes two functions `b -> c` (`f`) and `a -> b` (`g`), and returns a function of type `a -> c`, hiding the intermediate type `b` by passing it directly to the function `f`. As it does not capture the argument of type `a` in the argument list, this implementation uses a *lambda function* to capture it as `x`.

Alternatively, this could be written as:

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)
```

Note: *Prelude* is a standard library enabled by default for all Haskell programs.

---
## Higher order functions

Wikipedia:
> *In mathematics and computer science, a higher-order function is a function that does at least one of the following:*
>
> - *takes one or more functions as an input*
> - *outputs a function*

For example, *map* and *fold* (reduce) are very common higher order functions in functional paradigm.

```haskell
> :type map
map :: (a -> b) -> [a] -> [b]

> :type foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
```
---
## Lazy evaluation

- most programming languages use eager evaluation
- but some start to implement lazy *generators*
- haskell is **lazy**: it will only compute a value when it is actually used
- memory requirements are less explicit and more difficult to reason about
- however, it is very convenient - allows "infinite" computation definitions (e.g. an infite list is
  expressed as `[1..]`):

```haskell
> take 5 [1..]
[1,2,3,4,5]
```

---

... or even

```haskell
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
```

.pull-right[![Right-aligned image](assets/fibonacci.jpg)]

This defines a Fibonacci sequence, as an infinite recursive function.

When calculating, e.g. `take 10 fibs`, it will return `[1,1,2,3,5,8,13,21,34,55]`, where:

1. it starts with `[1, 1]`
2. 2 was generated as 1+1 (sum of first and the second values of `fibs`)
3. once we have 2, it generates the next element - this time `1 + 2`
4. and so on, until we get 55 - which is the 10th element.

As we only requested 10 elements, Haskell will stop after calculating the 10th element and will
not be stuck calculating the sequence numbers forever, even
though there is no exit condition defined in the function `fibs`.

---
# Thanks!

Thanks for reading and now go do haskell!
---
---

# Generic language constructs


---
---
## Type alias

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
type Deck = [Card]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defines a type alias. The data can still be accessed using the original type.

---
## Newtype

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
newtype Deck = Deck [Card]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A combination of `data` and `type` - the usage of the resulting type is
that of a `data` type, however the runtime is of a type alias.



---
## Guards!

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
alg :: [Integer] -> Integer -> Integer
alg (x:x':xs) a = if x == a then x' + alg xs x'
                  else alg (x':xs) x'
alg [_] _ = 0
alg [] _ = 0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## Let .. in

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
myFunction :: Int
myFunction = let x = 1
             in x + 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## Where

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
myFunction :: Int
myFunction = increasedX
    where x = 1
          increasedX = x + 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## Do notation

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
myFunction :: IO Int
myFunction = do
    other <- otherFunction
    return $ 1 + other
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## Let inside do

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
myFunction :: IO Int
myFunction = do
    let x = 1
    return x
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
# Hello world


~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
main :: IO()
main = putStrLn "hello world"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
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

---
# Type system

---
## What's a type class?

- similar to interfaces/abstract classes in OO
- can have default implementation

```haskell
    countSame :: Eq a => a -> [a] -> Int
```

- `a` is a variable type - we can call it with any types that satisfy the constraints (that have an
  instance of `Eq` type class defined for them).
- `Eq` is a typeclass that we require for the type `a`. It is defined by the haskell library, and
specifies that the equality functions for the type `a` are defined (`==`), otherwise it will not
compile.

Given the above function signature, we can say that:

- we only require to be able to compare variables of type `a`
- we only **can** compare the variables of type `a`
- the same type `a` has to be passed to the 1st and 2nd params

---

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
class  Eq a  where
    (==), (/=)           :: a -> a -> Bool

    x /= y               = not (x == y)
    x == y               = not (x /= y)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- we can choose which function to implement - `==` or `/=`, because the other one will then be able
  to use the default behaviour of negating the implemented one.

---
## Deriving a type class

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
data MyType = MyType Int deriving Eq

instance Ord MyType where
    (MyType a) <= (MyType b) = a <= b
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*(in this case, simply `deriving (Eq, Ord)` would also have worked)*

---
## Usage in functions

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
findLowerThan :: Ord a => a -> [a] -> [a]
findLowerThan measure = filter (< measure)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
# Elements of functional programming

---
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





---
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

---
## Monoid laws

- Identity law
    - `mappend mempty x` = `x`
    - `mappend x mempty` = `x`
- Associative law
    - `mappend x (mappend y z)` = `mappend (mappend x y) z`

---
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


---
## Functors

- functor allows mapping over them
- definition:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
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


---
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

---
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


---
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

---
## Monad laws

- identity:
    - `return a >>= k`  =  `k a`
    - `m >>= return`  =  `m`
- associativity:
    - `m >>= (\x -> k x >>= h)`  =  `(m >>= k) >>= h`

---
## Maybe monad

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
Just 1 >>= (\a -> return $ a+1)  -- or just "Just 1 >>= return . (+1)"
-- Just 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `do` notation is just syntactic sugar over `>>=`!

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
computation :: Maybe Int
computation = do
    a <- Just 1
    return $ a + 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
## IO monad

~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
echo :: IO ()
echo = getLine >>= putStrLn
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

---
# Summary

---
## You've seen

- overview of haskell ecosystem
- basic language constructs
- functional programming constructs

---
## Where to next?

- code!
- learn you a haskell for great good
- real world haskell
- /r/haskell
- code more!

---
## Thanks

Any questions?
