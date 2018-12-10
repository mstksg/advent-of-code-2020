
import           AOC
import           Control.Monad.Except
import           Data.Semigroup
import           System.Exit
import           Text.Printf
import qualified System.Console.ANSI  as ANSI

main :: IO ()
main = do
    putStrLn ""
    cfg <- configFile defConfPath
    out <- runExceptT $ do
      runOut <- mainRun cfg $ (defaultMRO TSAll)
          { _mroTest = True
          }
      let Sum totErrors = (foldMap . foldMap) (uncurry numErrors) runOut
      when (totErrors > 0) $ throwError [printf "Failed %d test(s)." totErrors]
    case out of
      Left e   -> do
        withColor ANSI.Vivid ANSI.Red $
          putStrLn "[ERROR]"
        mapM_ putStrLn e
        exitFailure
      Right () -> pure ()

numErrors
    :: Maybe Bool
    -> Either [String] String
    -> Sum Int
numErrors m e = contM m <> contE e
  where
    contM Nothing  = 0
    contM (Just r)
      | r         = Sum 0
      | otherwise = Sum 1
    contE (Left  es)
      | null es   = Sum 0
      | otherwise = Sum 1
    contE (Right _) = Sum 0
