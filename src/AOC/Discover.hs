{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : AOC.Discover
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Template Haskell for discovering all named challenges in a given
-- directory.
--

module AOC.Discover (
    mkChallengeMap
  , solutionList
  , ChallengeMap
  , ChallengeSpec(..)
  , solSpec
  , solSpecStr
  , solSpecStr_
  , charPart
  , challengeName
  ) where

import           AOC.Solver
import           AOC.Util
import           Advent
import           Data.Bifunctor
import           Data.Data
import           Data.Finite
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Traversable
import           Data.Void
import           GHC.Exts
import           Language.Haskell.Exts      as E
import           Language.Haskell.Names
import           Language.Haskell.TH        as TH
import           Language.Haskell.TH.Syntax (TExp(..))
import           Prelude
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Text.Read                  (readMaybe)
import qualified Data.Map                   as M
import qualified Hpack.Config               as H
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P

-- | A specification for a specific challenge.  Should consist of a day and
-- a lowercase character.
data ChallengeSpec = CS { _csDay  :: Day
                        , _csPart :: Part
                        }
  deriving (Show, Eq, Ord)

-- | A map of days to parts to solutions.
type ChallengeMap = Map Day (Map Part SomeSolution)

-- | Get a 'ChallengeSpec' from a given reified solution (name).
--
-- @
-- solSpec \'day02a == CS { _csDay = 1, _csPart = 'a' }
-- @
--
solSpec :: TH.Name -> ChallengeSpec
solSpec = solSpecStr_ . nameBase

solSpecStr :: String -> Either (P.ParseErrorBundle String Void) ChallengeSpec
solSpecStr = P.runParser challengeName ""

solSpecStr_ :: String -> ChallengeSpec
solSpecStr_ = either (error . P.errorBundlePretty) id . solSpecStr

instance IsString ChallengeSpec where
    fromString = solSpecStr_

type Parser = P.Parsec Void String

-- | Template Haskell splice to produce a list of all named solutions in
-- a directory. Expects solutions as function names following the format
-- @dayDDp@, where @DD@ is a two-digit zero-added day, and @p@ is
-- a lower-case letter corresponding to the part of the challenge.
--
-- See 'mkChallengeMap' for a description of usage.
solutionList :: FilePath -> Q (TExp [(Day, (Part, SomeSolution))])
solutionList dir = TExp
                  . ListE
                  . map (unType . specExp)
                <$> runIO (getChallengeSpecs dir)


-- | Meant to be called like:
--
-- @
-- mkChallengeMap $$(solutionList "src\/AOC\/Challenge")
-- @
mkChallengeMap :: [(Day, (Part, SomeSolution))] -> ChallengeMap
mkChallengeMap = M.unionsWith M.union
               . map (uncurry M.singleton . second (uncurry M.singleton))


specExp :: ChallengeSpec -> TExp (Day, (Part, SomeSolution))
specExp s@(CS (Day d) p) = TExp $ TupE
    [ VarE 'mkDay_ `AppE` LitE (IntegerL (getFinite d + 1))
    , TupE
        [ ConE (partCon p)
        , ConE 'MkSomeSol `AppE` VarE (mkName (specName s))
        ]
    ]
  where
    partCon Part1 = 'Part1
    partCon Part2 = 'Part2

specName :: ChallengeSpec -> String
specName (CS d p) = printf "day%02d%c" (dayInt d) (partChar p)

getChallengeSpecs
    :: FilePath                 -- ^ directory of modules
    -> IO [ChallengeSpec]       -- ^ all challenge specs found
getChallengeSpecs dir = do
    exts   <- defaultExtensions
    files  <- listDirectory dir
    parsed <- forM files $ \f -> do
      let mode = defaultParseMode { extensions    = exts
                                  , fixities      = Just []
                                  , parseFilename = f
                                  }
      res <- parseFileWithMode mode (dir </> f)
      case res of
        ParseOk x       -> pure x
        ParseFailed l e -> fail $ printf "Failed parsing %s at %s: %s" f (show l) e
    pure $ moduleSolutions parsed

defaultExtensions :: IO [E.Extension]
defaultExtensions = do
    Right (H.DecodeResult{..}) <- H.readPackageConfig H.defaultDecodeOptions
    Just H.Section{..} <- pure $ H.packageLibrary decodeResultPackage
    pure $ parseExtension <$> sectionDefaultExtensions

moduleSolutions :: (Data l, Eq l) => [Module l] -> [ChallengeSpec]
moduleSolutions = (foldMap . foldMap) (maybeToList . isSolution)
                . flip resolve M.empty


isSolution :: Symbol -> Maybe ChallengeSpec
isSolution s = do
    Value _ (Ident _ n) <- pure s
    Right c             <- pure $ P.runParser challengeName "" n
    pure c

challengeName :: Parser ChallengeSpec
challengeName = do
    _    <- P.string "day"
    dStr <- P.many P.numberChar
    dInt <- maybe (fail "Failed parsing integer") pure $
                readMaybe dStr
    dFin <- maybe (fail $ "Day not in range: " ++ show dInt) pure $
                mkDay dInt
    c    <- P.lowerChar
    p    <- maybe (fail $ printf "Part not parsed: %c" c) pure $
                charPart c
    pure $ CS dFin p

-- | Parse a 'Char' into a 'Part'
charPart :: Char -> Maybe Part
charPart 'a' = Just Part1
charPart 'b' = Just Part2
charPart _   = Nothing

