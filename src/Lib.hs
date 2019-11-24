module Lib
    ( startServer
    ) where
import Rest (runRest)

startServer :: IO ()
startServer = runRest
