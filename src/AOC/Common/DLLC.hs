
module AOC.Common.DLLC where
-- module AOC.Common.DLLC (
--   ) where

-- import           Data.Mutable
import           Control.Monad.Primitive
import           Data.Foldable
import           Control.Monad
import           Data.Void
import           Control.Monad.Trans.Class
import           Data.List.NonEmpty      (NonEmpty(..))
import           Data.Primitive.MutVar
import qualified Data.List.NonEmpty      as NE
import qualified Data.Conduino as C
import qualified Data.Conduino.Combinators as C

data Node s a = Node { nLeft :: MutVar s (Node s a), nItem :: a, nRight :: MutVar s (Node s a) }
  deriving Eq

newtype List s a = List { getList :: MutVar s (MutVar s (Node s a)) }
  deriving Eq

-- data List s a = List (MutVar s (List s a)) (Ref s a) (MutVar s (List s a))

cloneTop :: (PrimMonad m, PrimState m ~ s) => List s a -> m (List s a)
cloneTop (List x) = List <$> do
    newMutVar =<< readMutVar x

singleton
    :: (PrimMonad m, PrimState m ~ s)
    => a
    -> m (List s a)
singleton x = do
    n0 <- newMutVar $ undefined
    writeMutVar n0 $ Node n0 x n0
    List <$> newMutVar n0

fromList :: (PrimMonad m, PrimState m ~ s) => NonEmpty a -> m (List s a)
fromList (x :| xs) = do
    l0 <- singleton x
    traverse_ (`insertRight` l0) (reverse xs)
    pure l0

sourceRight
    :: (PrimMonad m, PrimState m ~ s)
    => List s a
    -> C.Pipe i a u m ()
sourceRight lst = do
    r0 <- lift $ readMutVar (getList lst)
    n0 <- lift $ readMutVar r0
    C.yield (nItem n0)
    go1 r0 (nRight n0)
  where
    go1 r0 = go
      where
        go r
          | r == r0   = pure ()
          | otherwise = do
              n <- lift $ readMutVar r
              C.yield (nItem n)
              go (nRight n)

sourceRightForever
    :: (PrimMonad m, PrimState m ~ s)
    => List s a
    -> C.Pipe i a u m r
sourceRightForever lst = do
    r0 <- lift $ readMutVar (getList lst)
    go r0
  where
    go r = do
      n <- lift $ readMutVar r
      C.yield (nItem n)
      go (nRight n)

sourceLeft
    :: (PrimMonad m, PrimState m ~ s)
    => List s a
    -> C.Pipe i a u m ()
sourceLeft lst = do
    r0 <- lift $ readMutVar (getList lst)
    n0 <- lift $ readMutVar r0
    C.yield (nItem n0)
    go1 r0 (nLeft n0)
  where
    go1 r0 = go
      where
        go r
          | r == r0   = pure ()
          | otherwise = do
              n <- lift $ readMutVar r
              C.yield (nItem n)
              go (nLeft n)

sourceLeftForever
    :: (PrimMonad m, PrimState m ~ s)
    => List s a
    -> C.Pipe i a u m r
sourceLeftForever lst = do
    r0 <- lift $ readMutVar (getList lst)
    go r0
  where
    go r = do
      n <- lift $ readMutVar r
      C.yield (nItem n)
      go (nLeft n)

sourceZip
    :: (PrimMonad m, PrimState m ~ s)
    => List s a
    -> C.Pipe i (a, a) u m ()
sourceZip lst = do
    r0 <- lift $ readMutVar (getList lst)
    n0 <- lift $ readMutVar r0
    go1 r0 (nLeft n0) (nRight n0)
  where
    go1 r0 = go
      where
        go rl rr
          | rl == r0  = pure ()
          | otherwise = do
              nl <- lift $ readMutVar rl
              nr <- lift $ readMutVar rr
              C.yield (nItem nl, nItem nr)
              go (nLeft nl) (nRight nr)


readOut
    :: (PrimMonad m, PrimState m ~ s)
    => List s a
    -> m [a]
readOut lst = C.runPipe $ sourceRight lst
                     C..| C.sinkList

readFocus :: (PrimMonad m, PrimState m ~ s) => List s a -> m a
readFocus = fmap nItem . readMutVar <=< readMutVar . getList

readAt :: (PrimMonad m, PrimState m ~ s) => Int -> List s a -> m a
readAt n lst = case compare n 0 of
    LT -> C.runPipe $ sourceLeftForever lst
                 C..| (replicateM_ (abs n) C.awaitSurely *> C.map id)
                 C..| C.awaitSurely
    EQ -> readFocus lst
    GT -> C.runPipe $ sourceRightForever lst
                 C..| (replicateM_ n C.awaitSurely *> C.map id)
                 C..| C.awaitSurely

seek
    :: (PrimMonad m, PrimState m ~ s)
    => (a -> Bool)
    -> List s a
    -> m (Maybe Int)
seek p lst = do
    r0 <- readMutVar (getList lst)
    n0 <- readMutVar r0
    if p (nItem n0)
      then pure (Just 0)
      else go1 r0 (nLeft n0) (nRight n0)
  where
    go1 r0 = go 1
      where
        go !i rl rr
          | rl == r0  = pure Nothing
          | otherwise = do
              nl <- readMutVar rl
              if p (nItem nl)
                then Just (negate i) <$ writeMutVar (getList lst) rl
                else do
                  nr <- readMutVar rr
                  if p (nItem nr)
                    then Just i <$ writeMutVar (getList lst) rr
                    else go (i + 1) (nLeft nl) (nRight nr)

    -- do
    -- r0 <- readMutVar (getList lst)
    -- n0 <- readMutVar r0
    -- (nItem n0:) <$> go1 r0 (nRight n0)
  -- where
    -- go1 r0 = go
    --   where
    --     go r
    --       | r == r0   = pure []
    --       | otherwise = do
    --           n <- readMutVar r
    --           (nItem n:) <$> go (nRight n)

insertRight
    :: (PrimMonad m, PrimState m ~ s)
    => a
    -> List s a
    -> m ()
insertRight x lst = do
    r0      <- readMutVar (getList lst)
    n0      <- readMutVar r0
    newNode <- newMutVar $ Node r0 x (nRight n0)
    writeMutVar r0 $ n0 { nRight = newNode }
    modifyMutVar (nRight n0) $ \nr -> nr { nLeft = newNode }

insertLeft
    :: (PrimMonad m, PrimState m ~ s)
    => a
    -> List s a
    -> m ()
insertLeft x lst = do
    r0      <- readMutVar (getList lst)
    n0      <- readMutVar r0
    newNode <- newMutVar $ Node r0 x (nLeft n0)
    writeMutVar r0 $ n0 { nLeft = newNode }
    modifyMutVar (nLeft n0) $ \nl -> nl { nRight = newNode }

rotateRight
    :: (PrimMonad m, PrimState m ~ s)
    => List s a
    -> m ()
rotateRight lst = do
    r0 <- readMutVar (getList lst)
    writeMutVar (getList lst) . nRight =<< readMutVar r0

rotateLeft
    :: (PrimMonad m, PrimState m ~ s)
    => List s a
    -> m ()
rotateLeft lst = do
    r0 <- readMutVar (getList lst)
    writeMutVar (getList lst) . nLeft =<< readMutVar r0

rotateN
    :: (PrimMonad m, PrimState m ~ s)
    => Int
    -> List s a
    -> m ()
rotateN n lst = case compare n 0 of
    LT -> replicateM_ (abs n) (rotateLeft lst)
    EQ -> pure ()
    GT -> replicateM_ n (rotateRight lst)

popRight
    :: (PrimMonad m, PrimState m ~ s)
    => List s a
    -> m a
popRight lst = do
    r0 <- readMutVar (getList lst)
    n0 <- readMutVar r0
    writeMutVar (getList lst) (nRight n0)
    modifyMutVar (nRight n0) $ \nr -> nr { nLeft  = nLeft n0 }
    modifyMutVar (nLeft  n0) $ \nl -> nl { nRight = nRight n0 }
    pure $ nItem n0

popLeft
    :: (PrimMonad m, PrimState m ~ s)
    => List s a
    -> m a
popLeft lst = do
    r0 <- readMutVar (getList lst)
    n0 <- readMutVar r0
    writeMutVar (getList lst) (nLeft n0)
    modifyMutVar (nLeft n0) $ \nl -> nl { nRight  = nRight n0 }
    modifyMutVar (nRight n0) $ \nr -> nr { nLeft = nLeft n0 }
    pure $ nItem n0




-- data Node s a = Node { nLeft :: MutVar s (Node s a), nItem :: a, nRight :: MutVar s (Node s a) }
--   deriving Eq

-- newtype List s a = List { getList :: MutVar s (MutVar s (Node s a)) }
