module Skelly (
  runAdd,
  runBuild,
  runClean,
  runRun,
  runTest,
) where

runAdd :: IO ()
runAdd = putStrLn "ADD"

runBuild :: IO ()
runBuild = putStrLn "BUILD"

runClean :: IO ()
runClean = putStrLn "CLEAN"

runRun :: IO ()
runRun = putStrLn "RUN"

runTest :: IO ()
runTest = putStrLn "TEST"
