{-# LANGUAGE BangPatterns #-}

-- ------------------------------------------------------------

module Hayoo.PackageRank
    ( DAGList
    , DAG
    , Ranking
    , Score
--    , dagFromList
--    , dagToList
--    , ranking
    , rankingStd
    )
where

import           Control.Applicative ( (<$>) )
import           Control.Arrow       ((>>>), (***), second)
import           Control.DeepSeq     (NFData)

import           Data.Monoid
import           Data.Map        (Map)
import qualified Data.Map        as M
import qualified Data.Map.Strict as SM
import           Data.Maybe      (fromMaybe, fromJust)
import qualified Data.List       as L
import           Data.Set        (Set)
import qualified Data.Set        as S

-- {-
import           Debug.Trace
-- -}

-- ------------------------------------------------------------
-- {- new version of rank computation
-- ------------------------------------------------------------

newtype  Rel a = Rel (DAG a, DAG a)

type     DAG a = Map a (Set a)

type DAGList a = [(a, [a])]

-- ----------------------------------------
--
-- Rel invariant and instances

inv :: (Ord a) => Rel a -> Bool
inv (Rel (s, p))
  = (L.sort . dagToList $ s)
    ==
    (L.sort . map (\ (x, y) -> (y, x)) . dagToList $ p)

instance Eq a => Eq (Rel a) where
  (Rel r1) == (Rel r2) = fst r1 == fst r2
  
instance Ord a => Monoid (Rel a) where
  mempty
    = empty

  Rel (s1, p1) `mappend` Rel (s2, p2)
    = Rel ( SM.unionWith S.union s1 s2
          , SM.unionWith S.union p1 p2
          )

instance (Ord a, Show a) => Show (Rel a) where
  show = show . toList

-- ----------------------------------------
--
-- conversions from DAG or DAGList to list of edges
  
dagToList :: DAG a -> [(a, a)]
dagToList = dagListToList . map (second S.toList) . SM.toList

dagListToList :: DAGList a -> [(a, a)]
dagListToList dl = [(x, y) | (x, ys) <- dl, y <- ys]

-- ----------------------------------------
--
-- Rel ops

empty :: Rel a
empty = Rel (SM.empty, SM.empty)
    
singleton :: (a, a) -> Rel a
singleton (x, y)
  = Rel ( SM.singleton x (S.singleton y)
        , SM.singleton y (S.singleton x)
        )

insert :: Ord a => (a, a) -> Rel a -> Rel a
insert (x, y)  (Rel (s, p))
  = Rel ( SM.insertWith S.union x (S.singleton y) s
        , SM.insertWith S.union y (S.singleton x) p
        )
    
fromList :: Ord a => [(a, a)] -> Rel a
fromList
  = L.foldl' (flip insert) empty

fromDAGList :: Ord a => DAGList a -> Rel a
fromDAGList = fromList . dagListToList

toList :: Rel a -> [(a, a)]
toList = dagListToList . succToDAGList

succToDAGList :: Rel a -> DAGList a
succToDAGList = map (second S.toList) . SM.toList . succs

predToDAGList :: Rel a -> DAGList a
predToDAGList = map (second S.toList) . SM.toList . preds

succs :: Rel a -> DAG a
succs (Rel r) = fst r

preds :: Rel a -> DAG a
preds (Rel r) = snd r

pr1 :: Rel a -> [a]
pr1 = SM.keys . succs

pr2 :: Rel a -> [a]
pr2 = SM.keys . preds

loops :: Ord a => Rel a -> [a]
loops (Rel r)
  = SM.keys . SM.filterWithKey S.member . fst $ r

member :: Ord a => (a, a) -> Rel a -> Bool
member (x, y) (Rel r)
  = y `S.member` (fromMaybe S.empty $ SM.lookup x (fst r))

-- --------------------
--
-- composition of binary relations, the core op for getting performance 
--
-- optimization: the outer loop of the join
-- is done over the map with the least size.
--
-- This is espessially effective if one of r1 or r2
-- is a single element rel, as in filterCycles

compose :: Ord a => Rel a -> Rel a -> Rel a
compose r1@(Rel (s1, p1)) r2@(Rel (s2, p2))
  | SM.size s2 <= SM.size p1 = composeR r1 r2
  | otherwise              = composeL r1 r2
                             
composeR :: Ord a => Rel a -> Rel a -> Rel a
composeR (Rel (s1, p1)) (Rel (s2, p2))
  = SM.foldrWithKey' ins1 empty s2  -- outer loop over succ of rel2
  where
    ins1 y' zs r
      = case SM.lookup y' p1 of
         Nothing -> r
         Just xs -> S.fold ins2 r zs  -- build the cart. prod over xs an zs
                    where
                      ins2 z r' = S.fold ins3 r' xs
                        where
                          ins3 x r'' = insert (x, z) r''

composeL :: Ord a => Rel a -> Rel a -> Rel a
composeL (Rel (s1, p1)) (Rel (s2, p2))
  = SM.foldrWithKey' ins1 empty p1 -- outer loop over pred of rel1
  where
    ins1 y xs r
      = case SM.lookup y s2 of
         Nothing -> r
         Just zs -> S.fold ins2 r zs  -- build the cart. prod over xs an zs
                    where
                      ins2 z r' = S.fold ins3 r' xs
                        where
                          ins3 x r'' = insert (x, z) r''
                          
infixr 9 <.>
(<.>) :: Ord a => Rel a -> Rel a -> Rel a
(<.>) = compose

transClosure :: (Show a, Ord a) => Rel a -> Rel a
transClosure r
  | r' == r   = r
  | otherwise = transClosure r'
  where
    r' = -- traceShow ("transClosure: r=" ++ show r) $
         r <.> r

-- ----------------------------------------

-- | compute the none refexive closure
-- of a relation given as list of pais
--
-- by removing all edges that would introduce a cycle
-- (a pair (x,x)) in the transitive closure
--
-- Which edges are removed depends on the order
-- in the input list.
--
-- This is a test for the filterCycles algorithm

acyclicRelFromList :: (Ord a, Show a) => [(a, a)] -> Rel a
acyclicRelFromList = snd . L.foldl' add1 (empty, empty)
  where
    add1 acc@(c, r) e@(x, y)
      | x == y    = -- traceShow ("loop: " ++ show x ++ "->" ++ show x) $
                    acc
      | (x, x) `member` cs
                  = -- traceShow ("cycle: " ++ show x ++ "->" ++ show y ++
                    --            "->...->" ++ show x) $
                    acc
      | otherwise = -- traceShow ("rel=" ++ show rs) $
                    (cs, insert e r)
      where
        xy = singleton e
        c1 = c  <.> xy  -- all x' connected with y via x 
        c2 = xy <.> c   -- all y' connected with x via y
        c3 = c1 <.> c   -- c <.> xy <.> c: all x' connected with all y' via x and y
        cs = c <> xy <> c1 <> c2 <> c3

-- ----------------------------------------
--
-- very much the same as acyclicRelFromList
-- but there's no Rel built
        
filterCycles :: (Ord a, Show a) => [(a, a)] -> [(a, a)]
filterCycles = fst . filterCycles'

filterCycles' :: (Ord a, Show a) => [(a, a)] -> ([(a, a)], [(a,a)])
filterCycles' = (reverse *** reverse) . snd . L.foldl' add1 (empty, ([], []))
  where
    add1 res@(r, (xs, ys)) e@(x, y)
      | x == y    = -- traceShow ("loop: " ++ show x ++ "->" ++ show x) $
                    (r, (xs, e : ys))
      | (x, x) `member` rs
                  = -- traceShow ("cycle: " ++ show x ++ "->" ++ show y ++
                    --            "->...->" ++ show x) $
                    (r, (xs, e : ys))
      | otherwise = -- traceShow ("rel=" ++ show rs) $
                    (rs, (e : xs, ys))
      where
        xy = singleton e
        r1 = r  <.> xy  -- all x' connected with y via x 
        r2 = xy <.> r   -- all y' connected with x via y
        r3 = r1 <.> r   -- r <.> xy <.> r: all x' connected with all y' via x and y
        rs = r <> r3 <> r2 <> r1 <> xy

-- ------------------------------------------------------------

type Ranking a = Map a Score
type Score     = Float

ranking :: (Ord a, Show a) => Score -> Rel a -> Ranking a
ranking w rel
  = -- traceShow r $
    ranks  -- dynamic programming: ranks is used within insertRank !!!
  where
    succs' = succs rel
    preds' = preds rel
    ranks  = foldl insertRank M.empty $ M.keys succs'

    insertRank r' k
      = M.insert k (w * (S.fold accRank (1/w) usedBy)) r'
      where
        usedBy           = fromMaybe S.empty . M.lookup k $ preds'
        accRank k' acc'  = (fromJust . M.lookup k' $ ranks) + acc'

rankingStd :: (Ord a, Show a) => DAGList a -> Ranking a
rankingStd
  -- use of strict map leads to complete evaluation of result
  = SM.map scale . ranking deflate . acyclicRelFromList . dagListToList
  where
    scale    = (/10.0) . fromInteger . round . (*10) . (+1.0) . logBase 2
    deflate  = 0.5

-- -}

-- ------------------------------------------------------------
{- old and buggy version of rank computation
-- ------------------------------------------------------------

type DAGList a                  = [(a, [a])]
type DAG a                      = Map a (Set a)
type Ranking a                  = Map a Score
type Score                      = Float

-- ------------------------------------------------------------

{-
-- | construct a directed graph form a list of nodes and successors.
--
--  This version does not check cycles, so it only works savely,
--  when this is checked before.

unsaveDagFromList               :: (Ord a, Show a) => DAGList a -> DAG a
unsaveDagFromList l             = -- traceShow l $
                                  map (second S.fromList) >>> M.fromList $ l
-- -}

-- | Construct a directed graph (DAG) form a list of nodes and successors.
--
-- The function checks for edges, that would introduce cycles, and deletes these
-- edges. So if there are cycles in the input list, the result depends on the
-- sequence of the pairs in the input list

-- with ghc-7.8.2 this function is evaluated repeatedly
-- a lot of times, visible by the traceShow in insertEdge,
-- but with the insertion of the traceShow (1. line) it's evalated only once,
-- as it should be and as it's done with ghc-7.6
--
-- substituting traceShow by a deepseq does not help
--
-- This behaviour only shows up in complete HayooIndexer,
-- not in a test program containing this code and a simple
-- main reading the DAG from a file.
-- Independent of the optimization flags, no -O, -O or -O2,
-- the error can't reproduced with the test prog
--
-- Its really a Heisenbug.

dagFromList                     :: (Ord a, Show a, NFData a) => DAGList a -> DAG a
dagFromList l                   = traceShow l $ -- <<<<<<<<<<<<<<<<<<<<<<<<<
                                  map (second S.fromList)
                                  >>>
                                  foldl (flip insEdges) M.empty $ l

insEdges                        :: (Ord a, Show a) => (a, Set a) -> DAG a -> DAG a
insEdges (x, ys) g
    | S.null ys                 = M.insertWith S.union x ys g
    | otherwise                 = S.fold (insertEdge x) g $ ys


-- these comments don't have influence on the bug,
-- it was a nice try

{-# NOINLINE dagFromList #-}
{-# NOINLINE insEdges #-}
{-# NOINLINE insertEdge #-}

-- ------------------------------------------------------------

-- | insert an edge from x to y into DAG g.
--
-- Check for possible cycles. Edges leading to cycles are discarded

insertEdge                      :: (Ord a, Show a) => a -> a -> DAG a -> DAG a
insertEdge x y g
    | x == y                    = traceShow ("cycle:", [x,y]) $
                                  g
    | existPath                 = traceShow ("cycle:", x:(head path)) $
                                  g
    | otherwise                 = M.insertWith S.union x (S.singleton y) g
    where
    path                        = take 1 $ allPaths g y x
    existPath                   = not . null $ path

-- ------------------------------------------------------------

-- | Compute all paths from one node to another.
--
-- this is used by insertEdge, when checking for cycles

allPaths                        :: (Ord a) => DAG a -> a -> a -> [[a]]
allPaths g                      = allPaths'
    where
    allPaths' x y
        | y `S.member` succs    = [[x,y]]
        | otherwise             = map (x:) . concatMap (flip allPaths' y) . S.toList $ succs
        where
        succs                   = fromMaybe S.empty . M.lookup x $ g

-- ------------------------------------------------------------

-- | Inverse to dagFromList

dagToList                       :: DAG a -> [(a, [a])]
dagToList                       = M.toList >>> map (second S.toList)

-- ------------------------------------------------------------

-- | Switch the direction in the DAG

dagInvert                       :: (Ord a) => DAG a -> DAG a
dagInvert                       = M.foldrWithKey invVs M.empty
    where
    invVs k ks acc              = S.fold invV acc1 $ ks
        where
        acc1                    = M.insertWith S.union k  S.empty         $ acc         -- don't forget the roots
        invV k' acc'            = M.insertWith S.union k' (S.singleton k) $ acc'

-- ------------------------------------------------------------

-- deepseq with g does not help, stil multiple evaluations of dagFromList and insertEdge

ranking                         :: (Ord a, Show a, NFData a) => Score -> DAG a -> Ranking a
ranking w g                     = -- traceShow r $
                                  r
    where
    g'                          = {- g `deepseq` -} dagInvert g
    r                           = foldl insertRank M.empty $ M.keys g
        where
        insertRank r' k         = M.insert k (w * (S.fold accRank (1/w) usedBy)) r'
            where
            usedBy              = fromMaybe S.empty . M.lookup k $ g'
            accRank k' acc'     = ( fromJust . M.lookup k' $ r ) + acc'

-- use of strict (SM) map leads to complete evaluation of result
rankingStd                      :: (Ord a, Show a, NFData a) => DAGList a -> Ranking a
rankingStd                      = SM.map scale . ranking deflate . dagFromList
    where
      scale                     = (/10.0) . fromInteger . round . (*10) . (+1.0) . logBase 2
      deflate                   = 0.5
-- -}
-- ------------------------------------------------------------
{- minimal test case

d1 :: DAG Int
d1 = dagFromList [(1,[2,3])
                 ,(2,[3,4])
                 ,(3,[]),(4,[])
                 ]
-- -}
-- ------------------------------------------------------------
