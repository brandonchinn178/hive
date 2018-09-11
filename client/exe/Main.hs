import React.Flux (reactRender)

import Hive.Client (app)

main :: IO ()
main = reactRender "app" app ()
