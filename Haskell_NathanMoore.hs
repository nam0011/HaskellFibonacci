{-
  Student: Nathan Moore
  Course: CS 424 - Intro to Operating Systems
  Professor: Harry Delugach
  Assignment: Program Two - Haskell program that has 3 functions
    1)Numbers as words - take an string input parse to int display that int as a word
    2)Fibanacci Sequence - ask user for integer and return all integers of Fib Sequence until given Integer
    3)Fibonacci Sequence' - ask user for integer and return all integers of Fib Sequence in works until given Integer
-}

--Runs to 'infinity' - Fibonacci Numbers
infFibs = 0 : 1 : zipWith (+) infFibs (tail infFibs)

--fibanacciSequence Function takes input from user and prints Integer type to console
fibonacciSequence :: Int -> String -> IO()
fibonacciSequence  iterator usrIn = do

  let numInt = read usrIn :: Integer
  let whileFibs = takeWhile (<= numInt) infFibs
  let listLength = length whileFibs
  if (iterator < (listLength - 1))
    then do
      print (whileFibs !! iterator)
      fibonacciSequence (iterator+1) usrIn
  else
    putStrLn " "

--fibanacciSequence' Function takes input from user, creates a list of numbers passes this list to numsAsWords functions
--allows numsAsWords to return String type to console
fibonacciSequence' :: Integer -> [Integer] -> numsAsWords -> Int -> IO()
fibonacciSequence' usrIn fibsToNum numFunc iterator = do
  let intLength = length fibsToNum
  let iterator' = 0
  numsAsWords iterator' (fibsToNum !! iterator)
  if (iterator < ((length fibsToNum)-1))
    then do
      fibonacciSequence' usrIn fibsToNum numFunc (iterator+1)
  else
    putStrLn " "

--numsAsWords Function takes in an Int type as an iterator and an Integer which will be parsed into a list
--then printed to the console as a String type
numsAsWords :: Int -> Integer -> IO()
numsAsWords iterator intList = do
  let intStr = show intList
  let intLength = length intStr
  let numAtIndex = (intStr !! iterator)
  if ((iterator < (intLength - 1)) && ((intStr !! (iterator+1)) /= ' '))
  then do
    case numAtIndex of
      '1' -> putStr "One-"
      '2' -> putStr "Two-"
      '3' -> putStr "Three-"
      '4' -> putStr "Four-"
      '5' -> putStr "Five-"
      '6' -> putStr "Six-"
      '7' -> putStr "Seven-"
      '8' -> putStr "Eight-"
      '9' -> putStr "Nine-"
      '0' -> putStr "Zero-"
  else
    case numAtIndex of
      '1' -> putStr "One"
      '2' -> putStr "Two"
      '3' -> putStr "Three"
      '4' -> putStr "Four"
      '5' -> putStr "Five"
      '6' -> putStr "Six"
      '7' -> putStr "Seven"
      '8' -> putStr "Eight"
      '9' -> putStr "Nine"
      '0' -> putStr "Zero"

  if (iterator < (intLength-1))
  then do
    numsAsWords (iterator + 1) intList
  else
    putStrLn " "


--BEGINNING ON MAIN ARGUMENT
main = do

  let iterator = 0 --general purpose iterator

  --block to enter an integer and have its digits displayed as Strings
  putStrLn "Enter an integer to display as a word: "
  numStr <- getLine
  let numInt = read numStr :: Integer
  numsAsWords iterator numInt
  putStrLn ""

  --call to display fibs until a num
  putStrLn "Enter an integer to display Fibonacci Sequence as Integers below the chosen number: "
  numStr<- getLine
  fibonacciSequence iterator numStr

  -- block to display numsAsWords of Fibonacci Sequence up to a given Number
  putStrLn "Enter an integer to display Fibonacci numbers as words below the chosen number: "
  numStr <- getLine
  let numInt = read numStr :: Integer
  let whileFibs = takeWhile (<= numInt) infFibs
  fibonacciSequence' numInt whileFibs numsAsWords iterator

{-
Haskell semmed easy to learn after watcing about five hours of youtube tutorials and following along I was able to
execute Program 2 in about two hours. Haskell has some great conventions for recursive calls and definitely made me
think more about the way I write my programs in other languages and how they can be better written to include more
recursion and ultimately better written and more readible to other users.
I though Haskell was fairly easy to both read and write comparitive to languages like R and Python it was very easy
to pick up. I definitely see why there is a growing movement of people using this language and believe I may jump on
this bandwagon for home projects.
The hardest part of this language was the heavy use of recursion but also became my favorite part fairly quick.
-}
