
module AOC.Common.RedBlack where

-- module AOC.Common.RedBlack (
--   ) where

import           Control.Monad.ST
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Foldable
import           Data.Mutable
import           GHC.Generics
import qualified Data.List.NonEmpty as NE


data Color = Red | Black
  deriving (Generic, Show)

instance Mutable s Color where
    type Ref s Color = MutVar s Color

data NodeData k a = NodeData
    { ndKey   :: k
    , ndColor :: Color
    , ndLeft  :: RBTree k a
    , ndVal   :: a
    , ndRight :: RBTree k a
    }
  deriving (Generic, Show, Functor, Foldable, Traversable)

data RBTree k a =
      Node (NodeData k a)
    | Leaf
  deriving (Generic, Show, Functor, Foldable, Traversable)

instance (Mutable s k, Mutable s a) => Mutable s (NodeData k a) where
    type Ref s (NodeData k a) = GRef s (NodeData k a)
instance (Mutable s k, Mutable s a) => Mutable s (RBTree k a) where
    type Ref s (RBTree k a) = GRef s (RBTree k a)

nodeBranch
    :: (Mutable s k, Mutable s a)
    => MutBranch s (RBTree k a) (NodeData k a)
nodeBranch = constrMB #_Node

leafBranch
    :: (Mutable s k, Mutable s a)
    => MutBranch s (RBTree k a) ()
leafBranch = constrMB #_Leaf

data TNE a b = TNE !b ![(a, b)]

insTNE :: a -> b -> TNE a b -> TNE a b
insTNE x y (TNE z zs) = TNE y ((x,z):zs)

treeInsert
    :: forall m s k a. (PrimMonad m, PrimState m ~ s, Mutable s k, Mutable s a, Ord k)
    => k
    -> a
    -> Ref s (RBTree k a)
    -> m (TNE Bool (Ref s (NodeData k a)))
    -- -> m (NonEmpty (Ordering, Ref s (NodeData k a)))   -- trace of ancestors
    -- -> m (NonEmpty (Ref s (RBTree k a)))   -- trace of ancestors
    -- -> m [Ref s (RBTree k a)]   -- trace of ancestors
treeInsert k x = go1
  where
    go1 :: Ref s (RBTree k a) -> m (TNE Bool (Ref s (NodeData k a)))
    go1 tRef = projectBranch nodeBranch tRef >>= \case
      Nothing -> do
        ndRef <- thawRef $ NodeData k Red Leaf x Leaf
        TNE ndRef [] <$ moveBranch nodeBranch tRef ndRef
      Just ndRef -> go2 ndRef
    go2 :: Ref s (NodeData k a) -> m (TNE Bool (Ref s (NodeData k a)))
    go2 ndRef = do
      k' <- freezePart (fieldMut #ndKey) ndRef
      case compare k k' of
        LT -> (insTNE False ndRef) <$> go1 (getMutPart (fieldMut #ndLeft ) ndRef)
        EQ -> TNE ndRef []      <$  copyPart (fieldMut #ndVal) ndRef x
        GT -> (insTNE True  ndRef) <$> go1 (getMutPart (fieldMut #ndRight) ndRef)

restore
    :: (PrimMonad m, PrimState m ~ s, Mutable s k, Mutable s a, Ord k)
    => TNE Bool (Ref s (NodeData k a))
    -> m ()
restore = \case
    TNE ndRef []              -> copyPart (fieldMut #ndColor) ndRef Black
    TNE ndRef ((cmp,pRef):ps) -> case cmp of
      -- this is the left branch, so uncle is the right
      -- NO this is wrong, that's the brother not the uncle UGH D:
      False -> do
        let uncle = getMutPart (fieldMut #ndRight) pRef
        c <- projectBranch nodeBranch uncle >>= \case
          Nothing   -> pure Black
          Just uRef -> freezePart (fieldMut #ndColor) uRef
        pure ()
      -- this is the right branch, so uncle is the left
      True  -> undefined
    
      
    -- ndRef :| []        -> copyPart (fieldMut #ndColor) ndRef Black
    -- ndRef :| (pRef:ps) -> do
      
-- restore
--     :: (PrimMonad m, PrimState m ~ s, Mutable s k, Mutable s a, Ord k)
--     => NonEmpty (Ref s (NodeData k a))
--     -> m ()
-- restore = \case
--     ndRef :| []        -> copyPart (fieldMut #ndColor) ndRef Black
--     ndRef :| (pRef:ps) -> do
      
    -- projectBranch nodeBranch t >>= \case
    --   Nothing -> error "hm.."
    --   Just (_, cRef, _, _, _) -> copyRef cRef Black
    -- t :| (u:us) -> projectBranch nodeBranch t >>= \case
    --   Nothing -> error "huh."

-- tester :: RBTree Int String
-- tester = runST $ do
--     t <- thawRef Leaf
--     for_ samps $ \(k,v) ->
--       treeInsert k v t
--     freezeRef t
--   where
--     samps =
--       [ (3, "hello")
--       , (4, "world")
--       , (9, "okay")
--       , (8, "what")
--       , (2, "so")
--       ]

