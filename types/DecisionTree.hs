module DecisionTree (build, predict, predict', drawDecisionTree,
                     Attribute(..),
                     DecisionTree(..)) where

import Data.Maybe (fromJust)
import Data.List hiding (partition)
import Data.Function (on)
import qualified Data.Map as Map

-- | The type for our DecisionTree
data DecisionTree n a b
  = NonLeaf       -- ^ No way to decide
  | Leaf b Double -- ^ Leafs have labels and entropy
  | forall r . (Show r,Ord r) => Node {
      attr   :: n,                      -- ^ the name of the attribute that this node asks for
      proj   :: a -> r,                 -- ^ projection which computes the value of the attribute
      values :: [r],                    -- ^ list of possible values
      child  :: r -> DecisionTree n a b -- ^ and has children which can be found with a value of the attribute
    }

data Attribute n a = forall r . (Show r,Ord r) => A {
       aName   :: n,                    -- ^ the attributes name, used for visualization in drawDecisionTree
       aProj   :: a -> r,               -- ^ a projection which extracts the value of the attribute from the input
       aValues :: [r]                   -- ^ list of possible values
     }

-- | Build a DecisionTree from the given training set. The number in the result is the accuracy.
build :: (Ord a, Ord b) => [Attribute n a] -> [(a,b)] -> (Double,DecisionTree n a b)
build atts dataset =
  let (count,t) = build 0 dataset
  in (fromIntegral count/fromIntegral (length dataset),t)
  where
    build count []      = (count,NonLeaf)
    build count dataset =
      case bestAttribute dataset atts of
        (0,  _         ) -> let cs = counts (map snd dataset)
                                (dominantLabel,matching) = Map.findMax cs
                            in (count+matching,Leaf dominantLabel (entropy cs))
        (inf,P n proj p) -> let (count',children) = mapAccumL build count p -- recursivly build the children
                            in (count'
                               ,Node {
                                  attr  = n,
                                  proj  = proj,
                                  values= Map.keys children,
                                  child = \a -> fromJust $ Map.lookup a children
                                })

-- | Predicts the result for a given input
predict :: DecisionTree n a b -> a -> Maybe b
predict t a = fmap fst (predict' t a)

-- | Predicts the result for a given input
predict' :: DecisionTree n a b -> a -> Maybe (b,Double)
predict' NonLeaf               _ = Nothing       -- we can't decide
predict' (Leaf b e)            _ = Just    (b,e) -- we reached a Leaf, done
predict' (Node n proj _ child) d = predict' (child (proj d)) d

drawDecisionTree :: (Show n,Show a,Show b) => DecisionTree n a b -> String
drawDecisionTree  = unlines . draw
  where
    draw NonLeaf                    = [""]
    draw (Leaf v e)                 = lines (show v ++ " " ++ show e)
    draw (Node n proj values child) = lines (show n) ++ drawSubTrees values
      where
        drawSubTrees []  = []
        drawSubTrees [v] =
            "|" : shift ("`- "++show v++" ") "   " (draw (child v))
        drawSubTrees (v:vs) =
            "|" : shift ("+- "++show v++" ") "|  " (draw (child v)) ++ drawSubTrees vs

        shift first other = zipWith (++) (first : repeat other)

data Partition n a b = forall r . (Show r,Ord r) => P n (a -> r) (Map.Map r [(a,b)])

-- | Partitions the dataset according to the possible values of the attribute.
-- If a particular value never appears in the dataset, then we get an empty
-- list for that value.
partition :: Ord a => [(a,b)] -> Attribute n a -> Partition n a b
partition dataset (A n project possibleValues) =
  P n project (foldl (\m k -> Map.insertWith (++) k [] m) grouped possibleValues)
  where
    grouped = groupWith (project.fst) (:[]) (++) dataset

-- | Computes the entropy of a Dataset
--
-- the Entropy is defined as: sum (p_i * log_2 p_i)
-- where p_i = |{ x | x has Label i}|/|Dataset|
entropy :: Ord b => Map.Map b Int -> Double
entropy cs = Map.foldr help 0 cs
  where
    n = fromIntegral $ sum cs
    help s acc | s/=0 = let p = fromIntegral s / n in acc-p*log p/log 2
    help _ _ = error "entropy: we are not supposed to get p=0"

-- we want to count how many Data we have for each label. Thus we group it with 1 as 
-- singleton value and add 1 whenever we find another Datum with the same label       
counts :: Ord b => [b] -> Map.Map b Int 
counts = groupWith id (const (1::Int)) (const succ)

-- | How much information does this Attribute give us for the given Dataset.
-- It is defined as 
--
-- entropy(set) - sum p_i * entropy {dat_i | dat has value i for attribute a}
information :: (Ord a,Ord b) =>  [(a,b)] -- ^ the data
            -> Attribute n a             -- ^ the attribute 
            -> (Double,Partition n a b)  -- ^ the information and the partition
information dataset attr = (inf,p)
  where
    p   = partition dataset attr
    ps  = case p of
            P _ _ m -> map (map snd) $ Map.elems m  -- the partitions, we're only interested in the labels
    inf = entropy (counts (map snd dataset)) - sum (map (\p -> (fromIntegral (length p) / n) * entropy (counts p)) ps)
    n   = fromIntegral $ length dataset

-- | Return the attribute which gives us greatest gain in information
bestAttribute :: (Ord a,Ord b) => [(a,b)] -> [Attribute n a] -> (Double,Partition n a b)
bestAttribute dataset = head.sortBy (compare `on` negatedInf) . map (\x -> information dataset x)
  where
    negatedInf (inf,_) = -inf

groupWith :: Ord k
          => (a -> k)      -- ^ how to extract a key
          -> (a -> v)      -- ^ how to make a value for the map
          -> (v -> v -> v) -- ^ how to fuse two values
          -> [a]           -- ^ the list we want to group
          -> Map.Map k v 
groupWith getKey singleton fuse = 
    foldl (\m x -> Map.insertWith fuse (getKey x) (singleton x) m) Map.empty
