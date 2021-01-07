{-# LANGUAGE OverloadedStrings #-}

import           AOC.Challenge.Day17
import           AOC.Common.Point
import           Conduit                  (runResourceT)
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Data.Conduit             (ConduitT, (.|), runConduit)
import           Data.Conduit.Process
import           Data.Foldable
import           Data.IntMap              (IntMap)
import           Data.IntSet              (IntSet)
import           Data.Semigroup
import           Data.Void
import           Graphics.Vega.VegaLite
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error
import           Text.Printf
import qualified Data.Aeson               as A
import qualified Data.ByteString          as BS
import qualified Data.Conduit.Combinators as C
import qualified Data.IntMap              as IM
import qualified Data.IntSet              as IS
import qualified Data.Serialize           as C
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T

main :: IO ()
main = do
    for_ [7..17] $ \d -> do
      (nxy, dats) <- loadData (d - 2)
      printf "d=%0d\n" d
      for_ (zip [0..] dats) $ \(t::Int, dat) -> do
        let coss = A.encode . fromVL $
              gridPlot IS.size ScLinear nxy (T.pack $ printf "Coset Count (d=%d, t=%d)" d t) dat
            muls = A.encode . fromVL $
              gridPlot (sum . map (finalWeight d . ixPascal d) . IS.toList)
                ScSymLog nxy (T.pack $ printf "Point Count (d=%d, t=%d)" d t) dat
        _ <- runResourceT $ runVegaLite
          (C.sourceLazy coss)
          (C.sinkFileCautious (printf "out/day17/cosets-d%02dt%d.png" d t))
          C.stderr
        runResourceT $ runVegaLite
          (C.sourceLazy muls)
          (C.sinkFileCautious (printf "out/day17/points-d%02dt%d.png" d t))
          C.stderr

gridPlot
    :: (IntSet -> Int)  -- ^ render
    -> Scale
    -> Int              -- ^ width of <x.y>
    -> T.Text           -- ^ title
    -> IntMap IntSet
    -> VegaLite
gridPlot f sc nxy ttl pts = toVegaLite
    [ title ttl []
    , background "white"
    , layer
        [ asSpec [ mark Rect [], encoding . rectEnc $ [] ]
        , asSpec [ mark Text [], encoding . textEnc $ [] ]
        ]
    , width 500
    , height 500
    , dat []
    ]
  where
    enc     = position X [ PName "x"
                         , PmType Ordinal
                         ]
            . position Y [ PName "y"
                         , PmType Ordinal
                         ]
    rectEnc = enc
            . color      [ MName "val"
                         , MmType Quantitative
                         , MScale [ SType sc ]
                         ]
    textEnc = enc
            . text       [ TName "labels"
                         , TmType Nominal
                         ]
    datRows :: [(Int, Int, Int)]
    datRows =
      [ (x, y, maybe 0 f (IM.lookup (x + y * nxy) pts))
      | x <- [0..nxy-1]
      , y <- [0..nxy-1]
      ]
    toDat (x, y, c) = Endo . dataRow $
      [ ("x", Number (fromIntegral x))
      , ("y", Number (fromIntegral y))
      , ("val", Number (fromIntegral c))
      , ("labels", Str (T.pack $ if c > 0 then show c else ""))
      ]
    dat = dataFromRows [] . appEndo (foldMap toDat datRows)

readFileMaybe :: FilePath -> IO (Maybe BS.ByteString)
readFileMaybe =
     (traverse (evaluate . force) . either (const Nothing) Just =<<)
   . tryJust (guard . isDoesNotExistError)
   . BS.readFile

loadData
    :: Int      -- ^ dim
    -> IO (Int, [IntMap IntSet])
loadData d = do
    res <- (either (const Nothing) Just . C.decode =<<) <$> readFileMaybe fp
    case res of
      Just r  -> pure r
      Nothing -> do
        inp <- parseAsciiSet (== '#') <$> readFile "data/17.txt"
        let out = runDay17 True True 6 d inp
            bds = maximum (concatMap toList (toList inp)) + 1
            nxy = bds + 12
        BS.writeFile fp (C.encode (nxy, out))
        pure (nxy, out)
  where
    fp = printf "cache/day17res/%02d.dat" d


runVegaLite
    :: MonadUnliftIO m
    => ConduitT () BS.ByteString m ()           -- ^ input stream (json)
    -> ConduitT BS.ByteString Void m a          -- ^ stdout stream
    -> ConduitT BS.ByteString Void m b          -- ^ stderr stream
    -> m (ExitCode, a, b)
runVegaLite cIn cOut cErr = sourceCmdWithStreams "vl2png"
     cIn
     cOut
     ( C.linesUnboundedAscii
    .| C.filter notAWarning
    .| C.unlinesAscii
    .| cErr
     )

notAWarning :: BS.ByteString -> Bool
notAWarning = not
            . ("warning" `T.isInfixOf`)
            . T.decodeUtf8

