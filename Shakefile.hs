{-# LANGUAGE BlockArguments #-}
import Development.Shake
--import Development.Shake.Command
import Development.Shake.FilePath

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} do
  want ["all"]

  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  phony "all" do
    hss <- getDirectoryFiles "" ["/Day*.hs"]
    let days = map (drop 3 . takeBaseName) hss
        results = map ("_build/result" </>) days
    need results

  "_build/input/*" %> \out -> do
    let day = takeFileName out
    putNormal ("Fetching input for day " <> day)
    cmd_ "curl" "-f" ("https://adventofcode.com/2019/day" </> day </> "input") "-H" "Cookie:session=53616c7465645f5f4e1cfd85c86422015e76f73767df144c9284c94aea578f13fddc2984fa46aa45224b34e01bd25924" (FileStdout out)

  "_build/result/*" %> \out -> do
    let day = takeFileName out
        input = "_build/input" </> day
        hs = "Day" <> day <.> "hs"
    putNormal ("Running day " <> day)
    need [input, hs]
    cmd_ "runhaskell" hs (FileStdin input) (FileStdout out)
    putNormal =<< liftIO (readFile out)
