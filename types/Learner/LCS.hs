module Learner.LCS(lcs, commonality, difference) where

import Data.List (mapAccumL, transpose)
import Data.Array
import Data.Array.ST
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import Control.Monad.ST

type State = Int
type WordGraph a = Array State ([Int],[(a,State)])

-- | Compute the longest common subsequence (lcs) for a list of sequences.
-- The outermost list in the result has more than one elements only if
-- there are several possible lcs. The lengths for all nested lists
-- are the same and always equal to the length of the lcs. The numbers
-- in the innermost list correspond to positions in the input sequences.
lcs :: Eq a => [[a]] -> [[[Int]]]
lcs [] = [[]]
lcs ws = maxpath (foldl1 intersect (map wordgraph ws))

-- | Preserve the common elements and replace the differences with
-- 'variables'. The first argument of the function should be
-- a potentially infinite list whose elements substitute all differences.
-- Example:
--    commonality (repeat '_') ["2abcx","adx","alpx"] ==> [["_a__x","a_x","a__x"]]
commonality :: Eq a => [a] -> [[a]] -> [[[a]]]
commonality vars ws =
  [zipWith (match vars 1) (transpose xs) ws | xs <- lcs ws]
  where
    match vars i []     w = []
    match vars i (j:js) w
      | i == j     = (w !! (i-1)) : match vars (i+1) js w
      | otherwise  = case vars of
                       (var:vars) -> var : match vars (i+1) (j:js) w

-- | Preserve the different elements and replace the common ones with
-- 'variables'. The first argument of the function should be
-- a potentially infinite list whose elements substitute all commonalities.
-- Example:
--    difference (repeat '_') ["2abcx","adx","alpx"] ==> [["2_bc_","_d_","_lp_"]]
difference :: Eq a => [a] -> [[a]] -> [[[a]]]
difference vars ws =
  [zipWith (match vars 1) (transpose xs) ws | xs <- lcs ws]
  where
    match vars i []     w = []
    match vars i (j:js) w
      | i == j     = case vars of
                       (var:vars) -> var : match vars (i+1) js w
      | otherwise  = (w !! (i-1)) : match vars (i+1) (j:js) w
{-
-- substDiff :: Eq a => [a] -> [[a]] -> [[[a]]]
substDiff vars ws =
  [match (repeat 1) xs ws | xs <- lcs ws]
  where
    match cur []     ws = ([],[])
    match cur (x:xs) ws
      | all null vs     = (c:patt,vss)
      | otherwise       = (c:patt,vs : vss)
      where
        c = head (head ws')
        (vs,next,ws') = unzip3 (zipWith3 substring cur x ws)
        (patt,vss)    = match next xs ws'

    substring i j w =
      case splitAt (j-i) w of
        (vs,_:ws) -> (vs,j+1,ws)
-}
wordgraph :: [a] -> WordGraph a
wordgraph xs = runSTArray (newArray (0,length xs) ([],[]) >>= iterate1 0 xs)
  where
    iterate1 i []     graph = do
      writeArray graph i ([i],[])
      return graph
    iterate1 i (x:xs) graph =
      iterate2 i i (x:xs) graph >>= iterate1 (i+1) xs

    iterate2 i j []     graph = return graph
    iterate2 i j (x:xs) graph = do
      (_,edges) <- readArray graph i
      writeArray graph i ([i],(x,j+1):edges)
      iterate2 i (j+1) xs graph

intersect :: Eq a => WordGraph a -> WordGraph a -> WordGraph a
intersect a b =
  let graph = intersect a b (Map.singleton (0,0) 0) [(0,(0,0))]
  in array (0,maximum (map fst graph)) graph
  where
    intersect a b statemap []                                 = []
    intersect a b statemap ((source,(asource,bsource)):stack) =
      let (apos,aedges) = a ! asource
          (bpos,bedges) = b ! bsource      
          ((statemap',stack'),edges) =
            mapAccumL transition (statemap,stack)
                      [(ka,atarget,btarget) | (ka,atarget) <- aedges, (kb,btarget) <- bedges, ka==kb]
      in (source,(apos++bpos,edges)) : intersect a b statemap' stack'
      where
        transition st@(statemap,stack) (k,asource,bsource) =
          case Map.lookup (asource,bsource) statemap of
            Nothing     -> let source = Map.size statemap
                               pair   = (asource,bsource)
                           in ((Map.insert pair source statemap,(source,pair):stack),(k,source))
            Just source -> (st,(k,source))

maxpath :: WordGraph a -> [[[Int]]]
maxpath graph
  | mm == 0   = [[]]
  | otherwise = endpoints >>= backtrace []
  where
    maxlens = runSTArray (newArray (bounds graph) (0,[]) >>= update 1 IntSet.empty [0])
    (mm,endpoints) = uncurry (maxima 0 []) (bounds maxlens)

    update step new []               maxlens
      | IntSet.null new = return maxlens
      | otherwise       = update (step+1) IntSet.empty (IntSet.toList new) maxlens
    update step new (source:sources) maxlens = do
      new <- propagate new (snd (graph ! source))
      update step new sources maxlens
      where
        propagate new []                 = return new 
        propagate new ((_,target):edges) = do
          (maxlen,parents) <- readArray maxlens target
          if maxlen < step
            then do writeArray maxlens target (step,[source])
                    propagate (IntSet.insert target new) edges
            else if maxlen == step
                   then do writeArray maxlens target (step,source:parents)
                           propagate new edges
                   else propagate new edges
        
    maxima m is i n
      | i <= n    =
          let (step,_) = maxlens ! i
          in maxima (max m step) (if step==mm then i:is else is) (i+1) n
      | otherwise = (m,is)

    backtrace path 0     = return path
    backtrace path state = do
      source <- snd (maxlens ! state)
      backtrace (fst (graph ! state):path) source
