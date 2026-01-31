module Learner.LCS(lcs) where

import Data.List (mapAccumL)
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
lcs ws = maxpath (foldl1 intersect (map wordgraph ws))

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
      | i < n     =
          let (step,_) = maxlens ! i
          in maxima (max m step) (if step==mm then i:is else is) (i+1) n
      | otherwise = (m,is)

    backtrace path 0     = return path
    backtrace path state = do
      source <- snd (maxlens ! state)
      backtrace (fst (graph ! state):path) source
