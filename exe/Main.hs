{-# LANGUAGE CPP #-}

import Language.Javascript.JSaddle.Warp (run)

import Hive.Client (app)

preMain :: IO ()
#ifdef ghcjs_HOST_OS
preMain = return ()
#else
preMain = putStrLn "Running on port 3000..."
#endif


main :: IO ()
main = preMain >> run 3000 app
