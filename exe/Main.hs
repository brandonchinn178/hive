import Language.Javascript.JSaddle.Warp (run)

import Hive.Client (app)

main :: IO ()
main = do
  putStrLn "Running on port 3000..."
  run 3000 app
