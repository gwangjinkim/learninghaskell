# file:///home/josephus/Downloads/Beginning%20Haskell.%20a%20project-based%20approach%20by%20Mena%20A.S.%20(z-lib.org).pdf

install in linux

sudo apt install haskell-platform

(you install haskl unix

install eclipse



-- sudo apt install ghc

-- in ghci
-- multiline function definitions put by
-- :{
-- }:

-- learn X in Y minutes
-- https://learnxinyminutes.com/docs/haskell/


-- Single Line comments start with two dashes
{- Multiline comments can be enclosed
   in a block like this -}

--------------------------------------
-- 1. Primitiv Datatyps and Operators
--------------------------------------

-- numbers
3 -- 3

-- Math is as you woul expect
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- Division is not integer division by default
35 / 4 -- 8.75

-- integer division

35 `div` 4 -- 8

-- boolean values are primitives
True
False

-- boolean operations
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- `not` is a function taking one value
-- Haskell doesn't need parantheses for function calls
-- all the args args are listed after the function
-- func arg1 arg2 arg3 ...

-- strings and characters
"This is a string."
'a' -- character
'You can't use single quotes for strings.' -- error!

-- strings can be concatenated
"Hello " ++ "world!" -- "Hello world!"

--  string is a list of characters
['H', 'e', 'l', 'l', 'o'] -- "Hello"

-- Lists can be indexed with the `!!` operator followed by an index
"This is a string" !! 0 -- 'T'

-----------------------------------------
-- 2. Lists and Tuples
-----------------------------------------

-- Every element in a list must have the same type.
-- These two lists are equal:
[1, 2, 3, 4, 5]
[1..5]

-- Ranges are versatile.
['A'..'F'] -- "ABCDEF"

-- You can create a step in a range.
[0,2..10] -- [0, 2, 4, 6, 8, 10]
[5..1] -- [] (Haskell defaults to incrementing)
[5,4..1] -- [5, 4, 3, 2, 1]

-- indexing into a list
[1..10] !! 3 -- 4 (zero-based indexing)

-- infinite lists are possible! due to "lazyness"
[1..] -- a list of natural numbers
[1..] !! 999 -- 1000

-- join two lists
[1..5] ++ [6..10]

-- add to head of a list
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- more list operations
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- list comprehensions
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- with a conditional
[x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]

-- every element in a tuple can be a different type,
-- but a tuple has fixed length.
("haskell", 1)

-- accessing elements of a pair (tuple of length 2)
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

-- pair lement accessing doesn't work on n-tuples
snd ("snd", "can't touch this", "da na na") -- error!

------------------------------------
-- 3. Functions
------------------------------------
-- a simple function that takes two variables
add a b = a + b

-- if you are using ghci (haskell interpreter)
-- you need `let`
let add a b = a + b

-- use function
add 1 2 -- 3

-- as infix
1 `add` 2 -- 3

-- you can also define functions that have no letters! (own operators)
-- e.g. integer division operator
(//) a b = a `div` b
35 // 4 -- 8

-- Guards: an easy way to do branching in functions
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

-- Pattern matching is similar.
-- here we have three equations defining fib.
-- haskell uses automatically the first matching pattern
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- so `=` is like `if .. then ..`

-- pattern matching on tuples
sndOfTuple (_, y, _) = y -- use wild card (_) to bypass unused value

-- pattern matching on list. first and rest
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

--- as a lisper, this is nice

-- lambdas are created with backslash followed by args
myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- use fold (`inject` in some languages)
-- foldl1 uses first value in list as initial value
-- for accumulator
foldl1 (\acc x -> acc + x) [1..5] -- 15

------------------------------------------
-- 4. More functions
------------------------------------------

-- partial application:
-- if you don't pass in all args to a func
-- it gets "partially applied"
-- it returns a function taking rest of args

add a b = a + b
foo = add 10
foo 5 -- 15

-- another way to write the same thing
foo = (10+)
foo 5 -- 15

-- function composition using chainer operator `.`
foo = (4*) . (10+) -- add 10 and then multiply by 4

-- 4*(10+5) = 60
foo 5 -- 60

-- priority operator `$`
-- function call has highest possible priority of 10, left-associative
-- `$` makes priority 0 and is right-associative
-- using `$` one can give function calls as arguments
-- to function calls

-- before
even (fib 7) -- false

-- equivalently
even $ fib 7 -- false

-- composing function
even . fib $ 7 -- false

(even . fib) 7 -- false

-- so basically $ is saving paranthesis


--------------------------------------------
-- 5. Type signatures
--------------------------------------------

-- Haskell has a very strong type system,
-- and every valid expression has a type.

-- Some basic types:
5 :: Integer
"Hello" :: String
True :: Bool

-- Function have types too.

-- `not` takes a boolean and returns a boolean
not :: Bool -> Bool

add :: Integer -> Integer -> Integer

-- when you define a value, it's good practice to write its type above it:
double :: Integer -> Integer
double x = x * 2



---------------------------------------------
-- 6. Control Flow and If Expressions
---------------------------------------------

-- if-expressions
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- multiple lines, indentation is important
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- case expressions: how parse command line args
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- Haskell doesn't have loops; it uses recursion
-- map applies a function over every element in a list

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- make a for function using map
for array func = map func array

-- use it
for [0..5] & \i -> show i
-- or write it as:
for [0..5] show

-- to reduce a list use foldl or foldr
-- foldl <fn> <initial value> <list>
foldl (\x y -> 2*x + y) 4 [1, 2, 3] -- 43
-- this is the same as:
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl is left-handed, foldr is right-handed
foldr (\x y -> 2*x + y) 4 [1, 2, 3] -- 16
-- this is the same as:
(2 * 1 + (2 * 2 + (2 * 3 + 4)))


------------------------------------------
-- 7. Data Types
------------------------------------------

-- type constructor on left nd one or more data constructors on the right,, separated by pipe | symbol
-- sum//union type
-- each data constructor is a (possibly nullary)
-- function creating an obj of the type

-- this is essentially an enum
data Color = Red | Blue | Green

say :: Color -> String
say Red     = "You are Red!"
say Blue    = "You are Blue!"
say Green   = "You are Green!"

-- type constructor is used in type signature
-- data constructors used in body of the function
--   are pattern-matched against

-- next one: traditional container type holding two fields
-- in a type declaration, data constructions take
-- types as parameters
-- data constructors can have same name as type constructors
-- common when type only has a single dat constructor

data Point = Point Float Float

-- use in function ike:
distance :: Point -> Point -> Float
distance (Point x y) (Point x' y') = sqrt $ dx + dy
    where dx = (x - x') ** 2
          dy = (y - y') ** 2

-- Types can have multiple data constructors with arguments, too

data Name = Mononym String
          | FirstLastName String String
          | FullName String String String

-- To make things clearer we can use record syntax

data Point2D = CartesianPoint2D { x :: Float, y :: Flot }
             | PolarPoint {r ::Float, theta :: Float }

myPoint = CartesianPoint2D { x = 7.0, y = 10.0 }

-- Using record syntax automatically creates accessor functions
-- (the name of the field)

xOfMyPoint = x myPoint

-- xOfMyPoint is equal to 7.0

-- Record syntax also allows a simple from of update

myPoint' = myPoint { x = 9.0 }

-- myPoint' is CartesianPoint2D { x = 9.0, y = 10.0 }

myPoint'2 = CartesianPoint2D 3.3 4.0

-- it is also useful to pattern match data constructors in `case` expressions

distanceFromOrigin x = case x of (CartesianPoint2D x y) -> sqrt $ x ** 2 + y ** 2
                                 (PolarPoint2D r _) -> r

-- your data types can have type parameters too:

data Maybe a = Nothing | Just a

-- These are all of type Maybe
Just "hello"  -- of type `Maybe String`
Just 1        -- of type `Maybe Int`
Nothing       -- of type `Maybe a` for any `a`

-- for convenience we can also create type synonyms
--- with the `type` keyword

type String = [Char]

-- unlike `data` types, synonyms need no constructor
-- and can be used anywhere a synonymous data type
-- can be used.

type Weight = Float
type Height = Float
type Point = (Float, Float)

getMyHeightAndWeight :: Person -> (Height, Weight)
findCenter :: Circle -> Point
somePerson :: person
someCircle :: Circle
distance :: Point -> Point -> Float

-- the following compiles and runs without issue,
-- though doesn't make sense semantically
-- because type synonyms reduce to the same base types

distance (getMyheightAndWeight somePerson) (findCenter someCircle)



--------------------------------------
-- 8. Typeclasses
--------------------------------------

-- Typeclasses are one way Haskell does polymorphism
-- similar to interfaces in other languages
-- a typeclass defines a set of functions
-- that must work on any type that is in that typeclass.

-- Eq typeclass is for types whose instances can be tested for equality with one another.

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

-- this defines a typclass that requires two functions
-- (==) and (/=)
-- it also declase that one fution can be declared
-- in terms of another
-- So it is enough that *either* of one is defined
-- and the other will be 'filled in' based on the
-- typeclass definition

-- to make a type a member of a type class, the 
-- instance keyword is used

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- Now we can use (==) and (/=) with TrafficLight objs

canProceedThrough :: TrafficLight -> Bool
canProceedThrough t = t /= Red

-- you can NOT create an instance definition for a 
-- type synonym.

-- functions can be written to take typeclasses with 
-- type parameters, rather than types,
-- assuming that the function only relieds on features
-- of the typeclass

isEqual (Eq a) => a -> a -> Bool
isEqual x y = x == y

-- Note that x and y MUST be the same type, as they are both
-- defined as being of type parameter 'a'.
-- a typeclass does not state that different types in the typeclass can be mixed together.
-- so `isEqual Red 2` is invalid, even through 2 is an Int which is an instance of Eq

-- other common typeclasses are:
-- Ord for typs that can be ordered,
-- allowing to use >, <=, etc.
-- Read for types that can be create from a string representation
-- Show for types that can be converted to a string for display
-- Num, Real, Integral, Fractional for types that can do math
-- Enum for types that can be stepped through
-- Bounded fro types with a max and min

-- haskell can automatically make types part of
-- Eq, Ord, Read, Show, Enm, and Bounded
-- with the `deriving` keyword at the end of 
-- the type declaration

data Point = Point Float Float deriving (Eq, Read, Show)

-- in thi case it is NOT necessary to create an 'instance' definition


-----------------------------------------
-- 9. Haskell iO
-----------------------------------------

-- While IO can't be explained fully without explaining monads,
-- it is not hard to explain enough to get going.

-- when a Haskell program is executed, `main` is called.
-- It must return a value of type `IO a` for some type `a`. e.g.

main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue)
-- putStrLn has type String -> IO ()

-- It is easiest to do IO if you cn implement your program as
-- a function from String to String.
-- the function 
--     interact :: (String -> String) -> IO ()
-- inputs some text, runs a function on it, and prints out the

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- You can think of a value of type `IO ()` as representing a
-- sequence of actions for the computer to do, much like a
-- computer program written in an imperative language.
-- We can use the `do` notation to chain actions together.
-- e.g.

sayHello :: IO ()
sayHello = do
    putStrLn "What is your name?"
    name <- getLine -- this gets a line and gives it the name "name"
    putStrLn $ "Hello, " ++ name

-- Execise: write your own version of `interact` that only reads one line input.
-- the cod in `sayHello` will never be executed however.
-- the only action that ever gets executed is the value of `main`
-- to run `sayHello` comment out the above definition of `main`
-- and replace it with:
-- main = sayHello

-- Let's understand better how the function `getLine` we just used works.
-- its type is: getLine :: IO String
-- You can think of a value of type `IO a` as representing a computer program
-- that will generate a value of type `a` when executed
-- (in addition to anything else it does).
-- We can name and reuse this value using `<-`.
-- We can also make our own action of type `IO String`:

action :: IO String
action = do
    putStrLn "This is a line. Duh"
    input1 <- getLine
    input2 <- getLine
    -- The type of the `do` statemenet is that of its last line.
    -- `return` is not a keyword, but mereley a function
    return (input1 ++ "\n" ++ input2) 
    -- return :: String -> IO String
    
    -- WE can use this just like we used `getLine`:

main'' = do
    putStrLn "I will echo two lines!"
    result <- action
    putStrLn result
    putStrLn "This was all, folks!"
    
-- The type `IO` is an example of a "monad".
-- The way Haskell uses a monad to do IO allows it to be a purely functional language.
-- any function that interacts with the outside world (i.e. does IO)
-- gets marked as `IO` in its type signature.
-- this lets us reason about which functions are "pure" 
-- (don't interact with the outside world or modify state)
-- and which functions aren't.

-- this is a powerful feature, becuse it's easy to run pure functions
-- concurrently; so, cuncurrency in Haskell is very easy.

----------------------------------------------------
-- 10. The Haskell REPL
----------------------------------------------------

-- start by $ ghci
-- any new values need to be crated with `let`:

let foo = 5

-- see type by `:t`
:t foo
foo :: Integer

-- operators such as `+`, `:` and `$` are functions.
-- inspect their types by putting operator in parantheses

:t (:)
(:) :: a -> [a] -> [a]

-- get additional info by `:i`

:i (+)
class Num a where
    (+) :: a -> a -> a
    ...
    -- Defined in `GHC.Num`
infixl 6 +

-- you can also run any action of type `IO ()`

sayHello
What is your name?
Friend!
Hello, Friend!

-- there is a lot more to Haskell


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

cycle   
take 10 (cycle [1, 2, 3])

repeat
take 10 (repeat 5)

replicate 3 5       -- [5, 5, 5]


-- list comprehensions

[x*2 | x <- [1..10]]    -- [2,4..20]
[x*2 | x <- [1..10], x*2 >= 12]  -- [12, 14..20]

-- all numbers from 50 to 10 which are modulo 7 -> 3
[x | x <- [50..100], x `mod` 7 == 3] -- [52, 59, 66, 73, 80, 87, 94]


-- filter lists

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
-- last part is predicate
boomBangs [7..13]
["BOOM!", "BOOM!", "BANG!", "BANG!"]
-- include several predicates
[x | x <- [10..20], x /= 13, x /= 15, x /= 19] -- filters out these numbers
-- nested lists
[x*y | x <- [1,2,3], y <- [10, 100, 1000]] -- [10,20,30,100,200,300,1000,2000,3000]
-- all possible products which are > 50
[x*y | x <- [1,2,3], y <- [10,100,1000], x.y > 50]

let nouns = ["hobo","frog","pope"]
let adjectives = ["lazy", "grouchy", "scheming"]
[adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

{- ["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",  
"grouchy pope","scheming hobo","scheming frog","scheming pope"]
-}

-- own length
length' xs = sum [1 | _ <- xs]

-- use '_' for variables you don't use!

removeNonUppercase st = [c | c <- str, c `elem` ['A'..'Z']]

removeNonUppercase "Hahahaha! Ahahaha!" --> "HA"

-- nested list expressions
let xxs = [[1,2,3],[1,2,3,4],[1,1,2]]
[[x | x <- xs, even x] | xs <- xxs] 
--> [[2],[2,4],[2]]

-- tuples are like fixed sized lists, immutable
-- but in contrast to lists, they can bear elements of different types
-- you can't compare two tuples of different sizes, but you can compare lists of two different sizes
[[1,2],[1,2,3],[1]] -- works
[(1,2),(1,2,3),(1)] -- error!
[[1,2],["one",1]]   -- type error!
[(1,2),("one",1)]   -- works!

-- singleton lists exist but singleton tuples makes no sense

onlye on pairs (tple of length 2)!
fst
snd

-- prouce list of pairs
zip




let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
let rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 = c^2]

-- so within list expression it is a let* situation!

-- the ones with perimeter 24
let rightTringles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 = c^2, a+b+c=24]
rightTriangles' -- [(6,8,10)]



-- common pattern in functional programming:
-- start with a set of solutions,
-- then apply transformations to those solutions
-- then filter them until you get the right ones


-------------------------------------------------------
-- Types and Typeclasses
-------------------------------------------------------

-- haskell can infer number types

:t 'a'         -- type of 'a' --> 'a' :: Char
:t True        -- True :: Bool
:t "HELLO!"    -- "HELLO!" :: [Char] -- String
:t (True, 'a') -- (True, 'a') :: (Bool, Char)
:t 4 == 5      -- 4 == 5 :: Bool

-- :: "has type of"

-- function definitions preceded by type declaration

removeNonUppercase :: [Char] -> [Char] -- :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
-- actually superfluous because compiler can infer String

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- parameters and return type are separated by `->`

-- if you are unsure about type declaration,
-- write function first and then check it with `:t functionname`

-- common types:
Int -- on 32-bit machines -2147483648 to 2147483647
Integer -- really big numbers, Int however more efficient
Float   -- single precision
Double  -- double precision
Bool    -- True and False
Char    -- character
String  -- [Char]
-- and infinite numbers of tuple types
-- () is a type which cn have only a single value: ()

-- Type variables

-- head function takes list of any type and returns first element
:t head
head :: [a] -> a   -- a can be of any type

-- functions which have type vriables are called `polymorphic functions`
-- usually one gives type variables a, b, c, d ... although they could have more characters

:t fst
fst :: (a, b) -> a


-- Typeclasses 101

-- are like java interfaces, only better

:t (==)
(==) :: (Eq a) => a -> a -> Bool

-- equality operator == is a function, so ar pretty much all operators
-- +, *, -, /
-- if a function is comprised only of special characters, it is considered an infix function by default
-- if call it as prefix function or pass it to another function, surround it in parantheses

-- => everything before it: class constraint
-- equality function takes any two values that are of same type and returns a bool
-- the class constraint is that the type of the two values must be a member of the Eq class
-- (Eq a)

-- all standard Haskell types except for IO and functions are part of the Eq typeclass.

-- the `elem` function has a type of (Eq a) => a -> [a] -> Bool
-- because it uses == over a list to check whether some value is in it

-- some basic typeclasses:

Eq -- types supporting equality testing
   -- the functions its members implement are == and /=
   -- so the Eq class constraint for a type variable in afunction is
   -- that is uses == or /= somewhere inside its definition
   -- all types we mentioned previously except for functions are part of Eq
Ord -- is for types that have orering

:t (>)
(>) :: (Ord a) => a -> a -> Bool

-- all types so far except for functions are part of Ord
-- Ord covers all the standard comparing functions <, >, >=, <=
-- the `compare` function takes two Ord members of the same type and returns an ordering

Ordering is a type that can be GT, LT or EQ (greater than, lesser than, equal)
-- to be a member of Ord, a type must first have membership in the prestigious and exclusive Eq club.

"Abrakadabra" < "Zebra"   -- True
"Abrakadabra" `compare` "Zebra"  -- LT
5 >= 2  -- True
5 `compare` 3  -- GT

Show -- members can be presented as strings
     -- all types covered so far except for functions are part of Show
     -- the most used function dealing with Show typeclass is show
     -- it takes value whose type is member of how and presents us as string

show 3  -- "3"
show 5.334 -- "5.334"
show True  -- "True"

Read - sort of opposite typeclass of Show
     -- read function takes a string and returns a type which is a member of Read
    
read "True" || False  -- True
read "8.2" + 3.8      -- 12.0
read "5" - 2           -- 3
read "[1,2,3,4]" ++ [3] -- [1,2,3,4,3]

read "4" -- ambiguous type

:t read
read :: (Read a) => String -> a

read "4" :: Int  -- 5
read "4" :: Float -- 5.0
(read "5" :: Float) * 4  -- 20.0
read "[1,2,3,4]" :: [Int] -- [1,2,3,4]
read "(3, 'a')" :: (Int, Char)  -- (3, 'a')












































-------------------------------------------------
-- Monads -- "typed pipes"
-------------------------------------------------
-- https://wiki.haskell.org/All_About_Monads

data Maybe a = Nothing | Just a

country = Just "China"
lookupAge :: DB -> String -> Maybe Int 
-- Maybe Int is a polymorphic Maybe container
-- olding an Int value or Nothing
-- Maybe String would be a Maybe container holding a String value or Nothing
-- make the type of the container polymorphic:
-- "m a"

{- monad: type constructor (call it m), a function that builds value of that type
          a -> m a
          and a function that combines values of that type with computations
          for values of that type
          m a -> (a -> m b) -> m b
          
          Note that the container is the same.





























