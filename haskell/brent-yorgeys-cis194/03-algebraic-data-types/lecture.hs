-- We can import specific functions for modules
import Data.Char ( toUpper )

main :: IO ()
main = putStrLn $ map toUpper "lecture three!"


{----------------------
-  Enumeration Types  -
----------------------}

-- We can create enumeration types in Haskell
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show -- And the compiler will magically allow us to convert
                -- members of this type to strings with this

shoe :: Thing
shoe = Shoe

listOThings :: [Thing]
listOThings = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _    = True


{------------------------
-  Beyond enumerations  -
------------------------}

-- Enums are a special case of algebraic data types.

-- Here Failure and OK are the constructors of FailableDouble
data FailableDouble = Failure
                    | OK Double
  deriving Show

ex01 = Failure
ex02 = OK 3.4 -- OK takes an arg, so without a value it's not an instance
              -- of type Failabledouble without one.

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK $ x / y

-- We can pattern match on types
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK x)  = x

data Person = Person String Int Thing
  deriving Show

richard :: Person
richard = Person "Richard" 32 Ship

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ x _) = x


{--------------------------
-  Some Pattern Matching  -
--------------------------}

-- We can retain the entire matched structure with var@(matching stuff)
someInfo :: Person -> String
someInfo person@(Person n _ _) = "The name of ("++ show person ++") is " ++ n

-- Types in pattern matching can nested
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're awesome."
checkFav (Person n _ _)          = n ++ ", your taste is rubbish"


{---------------------
-  Case expressions  -
---------------------}

-- Case expessions are constructions for performing pattern matching

ex03 = case "Hello" of
  []      -> 3
  ('H':s) -> length s
  _       -> 7


-- We could redefine the failureToZero function with case
failureToZero2 :: FailableDouble -> Double
failureToZero2 x = case x of
                    Failure -> 0
                    OK d    -> d

{-------------------------
-  Recursive Data Types  -
-------------------------}

-- Data types can be recursive!
-- Look, a list!
data OurList = Nil
             | Cons Int OurList
  deriving Show

aList :: OurList
aList = Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 Nil

-- And a tree!
data IntTree = Leaf Int
             | Node IntTree Int IntTree
  deriving Show

aTree :: IntTree
aTree = Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5))
