{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day23 where

-- module AOC.Challenge.Day23 (
--     day23a
--   , day23b
--   ) where

import           AOC.Prelude

-- import qualified Data.Vector.Sized           as V
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Align
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List.PointedList          (PointedList(..))
import           Data.Primitive.MutVar
import           Data.These
import           GHC.TypeNats
import qualified AOC.Common.DLLC                as DLLC
import qualified Data.Conduino                  as C
import qualified Data.Conduino.Combinators      as C
import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector.Mutable.Sized      as MV
import qualified Data.Vector.Sized              as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

data Entry s = Entry { eVal :: Int, ePrev :: MutVar s Int }

solver
    :: (PrimMonad m, PrimState m ~ s)
    => Int      -- ^ max
    -> Int      -- ^ n
    -> NonEmpty Int
    -> m (DLLC.List s Int)
solver mx n xs = do
    lst <- DLLC.fromList xs
    for [1..n] $ \i -> do
      step mx lst
      when (i `mod` 10000 == 0) $ do
        bef <- C.runPipe $ DLLC.sourceLeft  lst C..| C.take 9 C..| (C.drop 1 >> C.sinkList)
        x   <- DLLC.readFocus lst
        aft <- C.runPipe $ DLLC.sourceRight lst C..| C.take 9 C..| (C.drop 1 >> C.sinkList)
        traceM $ show (i, reverse bef, x, aft)
    pure lst

step
    :: forall m s. (PrimMonad m, PrimState m ~ s)
    => Int
    -> DLLC.List s Int
    -> m ()
step mx lst = do
    e0 <- DLLC.readFocus lst
    DLLC.rotateRight lst
    grabbed  <- replicateM 3 (DLLC.popRight lst)
    seeker   <- DLLC.cloneTop lst
    let target = until (`notElem` grabbed) subWrap (subWrap e0)
    _ <- DLLC.seek (== target) seeker
    traverse_ (`DLLC.insertRight` seeker) (reverse grabbed)
  where
    subWrap x
      | x == 1    = mx
      | otherwise = x - 1

day23a :: _ :~> _
day23a = MkSol
    { sParse = NE.nonEmpty . map digitToInt
    , sShow  = fmap intToDigit
    , sSolve = \xs -> Just $ runST $ do
        lst <- solver 9 100 xs
        _ <- DLLC.seek (== 1) lst
        tail <$> DLLC.readOut lst
    }

day23b :: _ :~> _
day23b = MkSol
    { sParse = NE.nonEmpty . take 1_000_000 . (++ [10..]) . map digitToInt
    , sShow  = show
    , sSolve = \xs -> Just $ runST $ do
        lst <- solver 1_000_000 10_000_000 xs
        _ <- DLLC.seek (== 1) lst
        (*) <$> DLLC.readAt (-1) lst <*> DLLC.readAt 1 lst
    }




    -- findTarget grabbed seeker = do

-- step
--     :: forall m s. (PrimMonad m, PrimState m ~ s)
--     => Int
--     -> DLLC.List s (Entry s)
--     -> m ()
-- step mx lst = do
--     e0 <- DLLC.readFocus lst
--     DLLC.rotateRight lst
--     grabbed  <- replicateM 3 (DLLC.popRight lst)
--     seeker   <- DLLC.cloneTop lst
--     d        <- findTarget e0 grabbed seeker
--     -- ..&..@....i##&
--     -- ..&..@##&....i
--     -- d = -6
--     -- p_0 = -5
--     -- p_1 = -8
--     -- q_0 = -11
--     -- q_1 = -6
--     for_ grabbed $ \e -> modifyMutVar' (ePrev e) (subtract (d + 1))
--     traverse_ (`DLLC.insertRight` seeker) (reverse grabbed)
--   where
--     findTarget :: Entry s -> [Entry s] -> DLLC.List s (Entry s) -> m Int
--     findTarget (Entry x0 rp) grabbed lst = do
--         p <- readMutVar rp
--         DLLC.rotateN p lst
--         Entry y _ <- DLLC.readFocus lst
--         if y == expected
--           then pure p
--           else if any ((== expected) . eVal) grabbed
--                  then pure p
--                  else do
--                     d <- fromJust <$> DLLC.seek ((== expected) . eVal) lst
--                     modifyMutVar' rp (+ d)
--                     e' <- DLLC.readFocus lst
--                     (+(d+p)) <$> findTarget e' grabbed lst
--         -- if any ((== expected) . eVal) grabbed
--           -- then do
--             -- e' <- DLLC.readFocus lst
--             -- (+ d') <$> findTarget e' grabbed lst
--           -- else pure d'  -- we're done
--       where
--         expected = traceShowId $ subWrap x0
--     subWrap x
--       | x == 1    = mx
--       | otherwise = x - 1
--     -- findTarget grabbed seeker = do

-- MV.MVector n s Int        -- ^ last seen


--stepper :: Int -> PointedList Int -> PointedList Int
--stepper n !pl = PLC.next $ jumpTo lab pl'''
--  where
--    lab = PL._focus pl
--    Just (grabbed,pl') = runStateT (replicateM 3 (StateT rightGet)) (PLC.next pl)
--    dest = until (`notElem` grabbed) (subWrapper n) (subWrapper n lab)
--    pl'' = jumpTo dest pl'
--    pl''' = insertRights grabbed pl''


--data Found = FNo | FHere | FThere Found
--  deriving (Eq, Ord, Show)

--makeBaseFunctor ''Found

--findIx :: Eq a => a -> [a] -> Found
--findIx x = ana $ \case
--  [] -> FNoF
--  y:ys | x == y    -> FHereF
--       | otherwise -> FThereF ys

--foundIx :: Found -> Maybe Int
--foundIx = cata $ \case
--    FNoF -> Nothing
--    FHereF -> Just 0
--    FThereF i -> (+ 1) <$> i

--foundFirst :: Found -> Found -> Maybe (Either Int Int)
--foundFirst = \case
--    FNo   -> fmap Right . foundIx
--    FHere -> \_ -> Just (Left 0)
--    FThere x -> \case
--      FNo -> Left <$> foundIx (FThere x)
--      FHere -> Just (Right 0)
--      FThere y -> bimap (+ 1) (+ 1) <$> foundFirst x y

--dispVec :: Foldable f => f (Finite n) -> [Int]
--dispVec = map dispVal . toList

--dispVal :: Finite n -> Int
--dispVal = fromIntegral . (+ 1) . getFinite

--day23a :: _ :~> _
--day23a = MkSol
--    { sParse = (traverse packFinite =<<) . V.fromList @9 . map (subtract 1 . fromIntegral . digitToInt)
--    , sShow  = \(xs, i) -> finiteEnum (i+1) (i-1) <&> \j ->
--                intToDigit (fromIntegral (xs `V.index` j) + 1)
--    -- map ((+ 1) . getFinite) . toList
--    -- , sShow  = map intToDigit . gogo . jumpTo 1
--    , sSolve = \xs -> Just $ runST $ run2 100 xs
--    -- Just . (!!! 100) . iterate (stepper 9)
--    }
--  where
--    gogo (PointedList xs _ ys) = ys ++ reverse xs


---- day23a :: _ :~> _
---- day23a = MkSol
----     { sParse = PL.fromList . map digitToInt
----     , sShow  = map intToDigit . gogo . jumpTo 1
----     , sSolve = Just . (!!! 100) . iterate (stepper 9)
----     }
----   where
----     gogo (PointedList xs _ ys) = ys ++ reverse xs

--subWrap x
--  | x == 1 = 9
--  | otherwise = x - 1

--    -- go pl = pl'
--    --   where
--    --     grab =

--rightGet :: PointedList a -> Maybe (a, PointedList a)
--rightGet pl = (PL._focus pl,) <$> PLC.deleteRight pl

--day23b :: _ :~> _
--day23b = MkSol
--    { sParse = (traverse packFinite =<<)
--             . V.fromList @1000000
--             . take 1000000
--             . (++ [9..])
--             . map (subtract 1 . fromIntegral . digitToInt)
--    , sShow  = \(xs, i) ->  show $
--          dispVal (xs `V.index` (i-1)) * dispVal (xs `V.index` (i+1))
--    -- finiteEnum (i+1) (i-1) <&> \j ->
--    --             intToDigit (fromIntegral (xs `V.index` j) + 1)
--    -- map ((+ 1) . getFinite) . toList
--    -- , sShow  = map intToDigit . gogo . jumpTo 1
--    , sSolve = \xs -> Just $ runST $ run2 10_000_000 xs
--    }

---- day23b :: _ :~> _
---- day23b = MkSol
----     { sParse = PL.fromList . take 1000000 . (++ [10..]) .  map digitToInt
----     , sShow  = show . gogo . jumpTo 1
----     -- , sShow  = map intToDigit . tail . toList . until ((== 1) . PL._focus) PLC.next
----     -- , sShow  = map intToDigit . toList
----     -- , sSolve = Just . (!!! 10000000) . map (\x -> traceShow (showAround x) x) . iterate go
----     , sSolve = Just . (!!! 10000000) . map tracer . zip [0..] . iterate go
----     }
----   where
----     tracer (i, x) = if i `mod` 1000 == 0
----                         then trace (printf "%.2f%%" (100 * fromIntegral i / 10000000 :: Double)) $ traceShow (showAround x) x
----                         else x
----     gogo (PointedList xs _ ys) = head xs * head ys
----     go = stepper 1000000

--stepper :: Int -> PointedList Int -> PointedList Int
--stepper n !pl = PLC.next $ jumpTo lab pl'''
--  where
--    lab = PL._focus pl
--    Just (grabbed,pl') = runStateT (replicateM 3 (StateT rightGet)) (PLC.next pl)
--    dest = until (`notElem` grabbed) (subWrapper n) (subWrapper n lab)
--    pl'' = jumpTo dest pl'
--    pl''' = insertRights grabbed pl''

--subWrapper n x
--  | x == 1 = n
--  | otherwise = x - 1


--insertRights :: [a] -> PointedList a -> PointedList a
--insertRights zs (PointedList xs z ys) = PointedList xs z (zs ++ ys)

--jumpTo :: Int -> PointedList Int -> PointedList Int
--jumpTo n p0@(PointedList xs z ys)
--    | n == z = p0
--    | otherwise = case foundFirst (findIx n xs) (findIx n ys) of
--        Nothing       -> error "hey"
--        Just (Left  i) -> iterate PLC.previous p0 !!! (i + 1)
--        Just (Right i) -> iterate PLC.next p0 !!! (i + 1)


--subWrap2 x
--  | x == 1 = 1000000
--  | otherwise = x - 1

--showUpTo (PointedList xs x ys) = (reverse xs, x, take 15 ys)

--showAround (PointedList xs x ys) = (reverse (take 10 xs), x, take 10 ys)

--afterSingle :: PointedList Int
--Just afterSingle = PL.fromList $
--                    [8,1,9,4,2] ++ filter (\i -> i `mod` 4 /= 0) [10..999999]
--                          ++ [3,6,7,5]
--                          ++ [12,16..1000000]

--run2
--    :: (PrimMonad m, PrimState m ~ s, KnownNat n)
--    => Int                          -- number of steps
--    -> V.Vector n (Finite n)        -- original
--    -> m (V.Vector n (Finite n), Finite n)    -- items minus one, plus index of 1
--run2 n xs0 = do
--    ixes <- MV.new
--    for finites $ \i ->
--      MV.write ixes (xs0 `V.index` i) i
--    xs <- V.thaw xs0
--    ivar <- newMutVar 0
--    for [1 .. n] $ \i -> do
--      stepper2 ivar ixes xs
--      when (i `mod` 10000 == 0) $ do
--        j <- readMutVar ivar
--        bef <- traverse (MV.read xs) [j - 9 .. j - 1]
--        x   <- MV.read xs j
--        aft <- traverse (MV.read xs) [j + 1 .. j + 9]
--        traceM $ show (i, dispVal j, dispVec bef, dispVal x :: Int, dispVec aft)

--    -- replicateM n $ stepper2 ivar ixes xs
--    (,) <$> V.freeze xs <*> MV.read ixes 0

--finiteEnum :: KnownNat n => Finite n -> Finite n -> [Finite n]
--finiteEnum x y = go x
--  where
--    go i | i == y    = [y]
--         | otherwise = i : go (i + 1)


--stepper2
--    :: forall n m s. (PrimMonad m, PrimState m ~ s, KnownNat n)
--    => MutVar s (Finite n)                      -- ^ focus
--    -> MV.MVector n s (Finite n)     -- ^ index of items
--    -> MV.MVector n s (Finite n)     -- ^ items
--    -> m ()
--stepper2 ivar ixes xs = do
--    i       <- readMutVar ivar
--    lab     <- MV.read xs i
--    -- traceM "hi"
--    -- traceM . show . dispVec =<< V.freeze xs
--    let grabIx = [i+1, i+2, i+3]
--    grabbed  <- traverse (MV.read xs) grabIx
--    -- traceM . show $ dispVec grabbed
--    let target = until (`notElem` grabbed) (subtract 1) (lab - 1)
--    targetIx <- MV.read ixes target
--    -- traceM "hi"
--    -- traceM $ show (i, lab)
--    -- traceM $ show (targetIx, target)
--    -- traceM $ show (fromIntegral target + 1::Int, targetIx)

--    -- 0123456789
--    -- 901 vs 123456789
--    -- 8901 vs 12345678
--    -- 78901 vs 1234567
--    -- 678901 vs 123456
--    -- 5678901 vs 12345
--    let delta = targetIx - 4 - i
--        dir
--          | delta == 0      = Nothing
--          | delta < halfWay = Just True
--          | otherwise       = Just False

--    -- traceM . show $ (delta < halfWay)
--    -- time to pick the smaller gap

--    case dir of
--      Nothing -> do
--      -- i###@
--      -- i@###
--        for_ (zip [2,3,4] grabbed) $ \(j, g) -> do
--          writeTrack (i + j) g
--        writeTrack (i + 1) target
--      Just True -> do
--      -- i
--      -- i###....@
--      -- i....@###
--        for_ (finiteEnum (i+1) (targetIx-4)) $ \j -> do
--        -- for_ (traceShowId $ finiteEnum (i+1) (targetIx-4)) $ \j -> do
--          writeTrack j =<< MV.read xs (j + 3)
--        writeTrack (targetIx - 3) target
--        for_ (zip [-2,-1,0] grabbed) $ \(j, g) -> do
--          writeTrack (targetIx + j) g
--      Just False -> do
--      --       i
--      -- @.....i###
--      -- @###..i...

--      -- #  @..i##
--      -- .  @###..
--      -- 346725891
--      -- 846791325
--        for_ (reverse (finiteEnum (targetIx+4) (i+3))) $ \j -> do
--        -- for_ (traceShowId $ reverse (finiteEnum (targetIx+4) (i+3))) $ \j -> do
--          writeTrack j =<< MV.read xs (j - 3)
--        for_ (zip [1,2,3] grabbed) $ \(j, g) -> do
--          writeTrack (targetIx + j) g

--    -- case flipper $ compare (targetIx - 4) i of
--    --   LT -> do
--    --   --       i
--    --   -- @.....i###
--    --   -- @###..i...

--    --   -- #  @..i##
--    --   -- .  @###..
--    --   -- 346725891
--    --   -- 846791325
--    --     -- for_ (reverse (finiteEnum (targetIx+4) (i+3))) $ \j -> do
--    --     for_ (traceShowId $ reverse (finiteEnum (targetIx+4) (i+3))) $ \j -> do
--    --       writeTrack j =<< MV.read xs (j - 3)
--    --     for_ (zip [1,2,3] grabbed) $ \(j, g) -> do
--    --       writeTrack (targetIx + j) g
--    --   EQ -> do
--    --   -- i###@
--    --   -- i@###
--    --     for_ (zip [2,3,4] grabbed) $ \(j, g) -> do
--    --       writeTrack (i + j) g
--    --     writeTrack (i + 1) target
--    --   -- i
--    --   -- i###....@
--    --   -- i....@###
--    --   GT -> do
--    --     -- for_ (finiteEnum (i+1) (targetIx-4)) $ \j -> do
--    --     for_ (traceShowId $ finiteEnum (i+1) (targetIx-4)) $ \j -> do
--    --       writeTrack j =<< MV.read xs (j + 3)
--    --     writeTrack (targetIx - 3) target
--    --     for_ (zip [-2,-1,0] grabbed) $ \(j, g) -> do
--    --       writeTrack (targetIx + j) g

--    writeMutVar ivar . (+1) =<< MV.read ixes lab
--    -- i
--    -- ###@
--    -- @###
--    --
--    -- pure ()
--    -- for (zip [-2,-1,0] grabbed) $ \j ->
----    let delta = targetIx - (i+1)
----    case compare delta 0 of
----      LT -> undefined
----      EQ -> undefined
----    _
----    -- dest = until (`notElem` grabbed) (subWrapper n) (subWrapper n lab)
----    pure ()
--  where
--    halfWay = (maxBound :: Finite n) `div` 2
--    writeTrack j x = do
--      MV.write ixes x j
--      MV.write xs   j x
----    l = MV.length xs


---- toOne :: PointedList Int -> PointedList Int
---- toOne = until ((== 1) . PL._focus) PLC.next
