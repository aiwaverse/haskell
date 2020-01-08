import           Lesson21

main :: IO ()
main = do
  let pizza1      = Pizza 10 5
  let pizza2      = Pizza 15 2
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)
