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
  , solverNFData
  , deepInstance
  ) where

import           AOC.Solver
import           Advent
import           Control.Applicative
import           Control.DeepSeq
import           Language.Haskell.TH.Datatype
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor
import           Data.Data
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
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Hpack.Config               as H
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PL

-- | Big quick escape hatch if things explode in the middle of solving.
-- This will disable the check for NFData when using 'MkSomeSol' and assume
-- no NFData in every case.
checkIfNFData :: Bool
checkIfNFData = True
-- checkIfNFData = False

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
solSpec n = solSpecStr_ (nameBase n)

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
solutionList :: FilePath -> Code Q [(Day, (Part, SomeSolution))]
solutionList dir = Code $
         fmap (TExp . ListE)
       . traverse (fmap unType . specExp)
     =<< runIO (getChallengeSpecs dir)

-- | Meant to be called like:
--
-- @
-- mkChallengeMap $$(solutionList "src\/AOC\/Challenge")
-- @
mkChallengeMap :: [(Day, (Part, SomeSolution))] -> ChallengeMap
mkChallengeMap = M.unionsWith M.union
               . map (uncurry M.singleton . second (uncurry M.singleton))


specExp :: ChallengeSpec -> Q (TExp (Day, (Part, SomeSolution)))
specExp s@(CS d p) = do
    n <- lookupValueName (specName s)
    con <- case n of
      Nothing -> pure 'MkSomeSolWH
      Just n' -> do
        isNF <- solverNFData n'
        pure $ if isNF
                 then 'MkSomeSolNF
                 else 'MkSomeSolWH
    pure $ TExp $ TupE
      [ Just $ VarE 'mkDay_ `AppE` LitE (IntegerL (dayInt d))
      , Just $ TupE
          [ Just $ ConE (partCon p)
          , Just $ ConE con `AppE` VarE (mkName (specName s))
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
    Right H.DecodeResult{..} <- H.readPackageConfig H.defaultDecodeOptions
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
    dInt <- PL.decimal
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

-- | Check if a solver identifier is of type @A ':~>' B@, where @B@ is an
-- instance of 'NFData'.
solverNFData :: TH.Name -> Q Bool
solverNFData n
  | checkIfNFData = reify n >>= \case
      VarI _ (ConT c `AppT` a `AppT` _) _
        | c == ''(:~>) -> deepInstance ''NFData a
      _ -> pure False
  | otherwise     = pure False

-- | Check if a type is an instance of a class, unifying when possible
deepInstance
    :: TH.Name  -- ^ class
    -> TH.Type  -- ^ type
    -> Q Bool
deepInstance cn = fmap isJust . runMaybeT . deepInstance_ cn

deepInstance_
    :: TH.Name  -- ^ class
    -> TH.Type  -- ^ type
    -> MaybeT Q ()
deepInstance_ cn t = do
    insts <- maybe empty pure . NE.nonEmpty =<< lift (reifyInstances cn [t])
    forM_ insts $ \case
      InstanceD _ ctx instHead _ -> do
        uni <- lift $ unifyTypes [ConT cn `AppT` t, instHead]
        forM_ ctx $ \case
          AppT (ConT c) v -> deepInstance_ c (applySubstitution uni v)
          _               -> empty
      _                            -> empty
