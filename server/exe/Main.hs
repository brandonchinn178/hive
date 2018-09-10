import Network.Wai.Handler.Warp (run)

import Hive.Server (app)

main :: IO ()
main = run 3000 app
