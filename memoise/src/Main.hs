import Data.Time.Clock.POSIX
import Data.MemoTrie (memo)

-- Some function that is expensive to run
expensive :: Int -> Int
expensive n = length $ replicate (n * 50000) 'a'

-- Some recursive function that calculates our expensive function repeatedly
expensiveRec :: Int -> Int
expensiveRec 0 = 0
expensiveRec n = expensive n + expensiveRec (n-1)

-- The recursive function redefined to push the recursion into fix
expensiveRec' :: Int -> Int
expensiveRec' = fix expensiveFix

-- k stands for kontinuation and replaces the recursive call
expensiveFix :: (Int -> Int) -> Int -> Int
expensiveFix _ 0 = 0 -- Stops the recursion because it doesn't use k
expensiveFix k n = expensive n + k (n-1)

-- This does our recursion for us. Equivalent to fix f = f (fix f)
fix :: (a -> a) -> a
fix f = let x = f x in x

-- A version of fix that 'memoises' the recursive calls, 
-- i.e. remembers what the answer was for a given parameter
-- We use fix so that it handles the memoisation and the recursion together
memoFix :: ((Int -> Int) -> (Int -> Int)) -> (Int -> Int)
memoFix h = fix (memoise . h)

-- Takes a function and memoises it. This works because function calls are not
-- cache, but data is. What happens when you make a call to a function memoised
-- in this way is the list is indexed, which triggers the evaluation of the
-- supplied function for that particular value, returning some result to that
-- index of the list. When the memoised function is called in future it indexes
-- this list again, but this time the result has already been calculated, so it
-- immediately retrieves it. This all works thanks to lazy (or more accurately
-- call by value) evaluation. This only works for Int inputs in this case, because
-- that's how !! works, but better memoisation strategies are available.
memoise :: (Int -> a) -> (Int -> a)
memoise f = (map f [0 ..] !!)

-- In GHCi enter `:set +s` to get timing information, then run these expressions
-- and compare them

-- This takes a while
expensiveTest :: [Int]
expensiveTest = map (fix expensiveFix) testList

-- This is much faster (at the cost of higher memory residency
-- since we're not garbage collecting previously computed values)
expensiveTestMemo :: [Int]
expensiveTestMemo = map (memoFix expensiveFix) testList

testList :: [Int]
testList = [0..40]

main :: IO ()
main = do
  putStrLn "Non-memoised time"
  t0 <- getPOSIXTime
  print expensiveTest
  t1 <- getPOSIXTime
  let nonMemoTime = t1 - t0
  putStrLn $ "That took " ++ show nonMemoTime ++ " seconds"

  putStrLn "Memoised time"
  t2 <- getPOSIXTime
  print expensiveTestMemo
  t3 <- getPOSIXTime
  let memoTime = t3 - t2
  putStrLn $ concat ["That took ", show memoTime, " seconds for a speedup of "
                    , show (nonMemoTime / memoTime)
                    ]


-- P.S. For some reason this definition of memoise is slower, idk why:
memoise' :: (Int -> a) -> (Int -> a)
memoise' f n = map f [0 ..] !! n