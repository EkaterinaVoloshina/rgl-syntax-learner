module DecisionTree (build, buildC, buildD, predict, predict', drawDecisionTree, values,
                     Attribute(..),
                     DecisionTree(..)) where

import Data.Maybe (fromJust)
import Data.List hiding (partition)
import Data.Functor.Contravariant
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | The type for a DecisionTree
data DecisionTree n a b
  = Leaf b Double -- ^ Leafs have labels and entropy
  | forall r . Ord r => Decision {
      name     :: n r,                           -- ^ the name of the attribute that this node asks for
      proj     :: a -> r,                        -- ^ projection which computes the value of the attribute
      children :: Map.Map r (DecisionTree n a b) -- ^ and has children which can be found with a value of the attribute
    }
  | forall r . Ord r => DecisionC {
      name     :: n r,                           -- ^ the name of the attribute that this node asks for
      proj     :: a -> r,                        -- ^ projection which computes the value of the attribute
      key      :: r,                             -- ^ split threshold
      left     :: DecisionTree n a b,            -- ^ children with value smaller or equal to the threshold
      right    :: DecisionTree n a b             -- ^ children with value grater than the threshold
    }

instance Functor (DecisionTree n a) where
  fmap f (Leaf b e) = Leaf (f b) e
  fmap f (Decision  n proj children)     = Decision  n proj (fmap (fmap f) children)
  fmap f (DecisionC n proj k left right) = DecisionC n proj k (fmap f left) (fmap f right)

instance Applicative (DecisionTree n a) where
  pure x = Leaf x 0
  (Leaf f _) <*> dt = fmap f dt
  (Decision n proj children) <*> dt = Decision n proj (fmap (flip (<*>) dt) children)

instance Monad  (DecisionTree n a) where
  (Leaf x _) >>= f = f x
  (Decision n proj children) >>= f = Decision n proj (fmap (flip (>>=) f) children)

data Attribute n a
  = forall r . Ord r => A {             -- ^ A categorial attribute
       aName   :: n r,                  -- ^ the attributes name, used for visualization in drawDecisionTree
       aProj   :: a -> r                -- ^ a projection which extracts the value of the attribute from the input
    }
  | forall r . Ord r => AC {            -- ^ A continuous attribute
       aName   :: n r,
       aProj   :: a -> r
    }

instance Contravariant (Attribute n) where
  contramap f (A  n proj) = A  n (proj . f)
  contramap f (AC n proj) = AC n (proj . f)

-- | Build a DecisionTree from the given training set. The number in the result is the accuracy.
build :: Ord b => [Attribute n a] -> [(a,b)] -> ([(a,b)],DecisionTree n a b)
build atts []      = error "Cannot learn without data"
build atts dataset = build [] dataset
  where
    build matched dataset =
      case bestAttribute dataset atts of
        (0,  _         ) -> let cs = groupWith snd (:[]) (++) dataset
                                (dominantLabel,matched') = Map.findMax cs
                            in (matched'++matched,Leaf dominantLabel (entropy (fmap length cs)))
        (inf,P n proj p) -> let (matched',children) = mapAccumL build matched p -- recursivly build the children
                            in (matched'
                               ,Decision {
                                  name     = n,
                                  proj     = proj,
                                  children = children
                                })
        (inf,PC n proj k l r)
                         -> let (matched1,left)  = build matched  l -- recursivly build the children
                                (matched2,right) = build matched1 r -- recursivly build the children
                            in (matched2
                               ,DecisionC {
                                  name = n,
                                  proj = proj,
                                  key  = k,
                                  left = left,
                                  right= right
                                })


-- | Build a regression DecisionTree from the given training set. The number in the result is the average standard deviation.
buildC :: [Attribute n a] -> [(a,Double)] -> (Double, DecisionTree n a Double)
buildC atts []      = error "Cannot learn without data"
buildC atts dataset =
  let ((sum,count),t) = build (0,0) dataset
  in (sum/count,t)
  where
    build st@(sum,count) dataset =
      case bestAttributeC dataset atts of
        (0,  _         ) -> let (avg,dev) = avgDeviation (map snd dataset)
                            in ((sum+dev,count+1),Leaf avg dev)
        (inf,P n proj p) -> let (st',children) = mapAccumL build st p -- recursivly build the children
                            in (st'
                               ,Decision {
                                  name     = n,
                                  proj     = proj,
                                  children = children
                                })
        (inf,PC n proj k l r)
                         -> let (st1,left)  = build st  l -- recursivly build the children
                                (st2,right) = build st1 r -- recursivly build the children
                            in (st2
                               ,DecisionC {
                                  name = n,
                                  proj = proj,
                                  key  = k,
                                  left = left,
                                  right= right
                                })

-- | Build a DecisionTree from the given training set. The number in the result is the accuracy.
buildD :: Ord b => [Attribute n a] -> [(a,[b])] -> DecisionTree n a [b]
buildD atts []      = error "Cannot learn without data"
buildD atts dataset = build dataset
  where
    build dataset =
      case bestAttributeD dataset atts of
        (0,  _         ) -> let bs = map (\(_,b)->(-length b,b)) dataset
                                dominantLabel = snd (head (sortOn fst bs))
                                total = Set.size (Set.fromList (concatMap snd bs))
                                den   = - sum (map fst bs)
                            in Leaf dominantLabel (fromIntegral den / fromIntegral total)
        (inf,P n proj p) -> let children = Map.map build p -- recursivly build the children
                            in Decision {
                                  name     = n,
                                  proj     = proj,
                                  children = children
                               }
        (inf,PC n proj k l r)
                         -> let left  = build l -- recursivly build the children
                                right = build r -- recursivly build the children
                            in DecisionC {
                                  name = n,
                                  proj = proj,
                                  key  = k,
                                  left = left,
                                  right= right
                               }

-- | Predicts the result for a given input
predict :: DecisionTree n a b -> a -> Maybe b
predict t a = fmap fst (predict' t a)

-- | Predicts the result for a given input
predict' :: DecisionTree n a b -> a -> Maybe (b,Double)
predict' (Leaf b e)                 _ = Just    (b,e) -- we reached a Leaf, done
predict' (Decision  n proj children) d = Map.lookup (proj d) children >>= \dt -> predict' dt d
predict' (DecisionC n proj k l r)    d
  | proj d <= k = predict' l d
  | otherwise   = predict' r d

drawDecisionTree :: (forall r . n r -> String) -> (forall r . n r -> r -> String) -> (b -> String) -> DecisionTree n a b -> String
drawDecisionTree showName showValue showResult = unlines . draw
  where
    draw (Leaf v e)                 = lines (showResult v ++ " " ++ show e)
    draw (Decision n proj children) = lines (showName n) ++ drawSubTrees (Map.toList children)
      where
        drawSubTrees []      = []
        drawSubTrees [(k,v)] =
            let s = showValue n k
            in "|" : shift ("`- "++s++" ") ("    "++map (const ' ') s) (draw v)
        drawSubTrees ((k,v):xs) =
            let s = showValue n k
            in "|" : shift ("+- "++s++" ") ("|   "++map (const ' ') s) (draw v) ++ drawSubTrees xs
    draw (DecisionC n proj k l r)   = lines (showName n) ++ drawSubTrees k l r
      where
        drawSubTrees k l r =
          let s = showValue n k
          in
            "|" : shift ("+- <="++s++" ") ("|     "++map (const ' ') s) (draw l) ++
            "|" : shift ("`- > "++s++" ") ("      "++map (const ' ') s) (draw r)

    shift first other = zipWith (++) (first : repeat other)

values :: DecisionTree n a b -> [b]
values dt = traverse dt []
  where
    traverse (Leaf x _) xs = x:xs
    traverse (Decision  n proj children) xs = Map.foldr traverse xs children
    traverse (DecisionC n proj k l r) xs = traverse l (traverse r xs)

data Partition n a b
  = forall r . Ord r => P  (n r) (a -> r) (Map.Map r [(a,b)])
  | forall r . Ord r => PC (n r) (a -> r) r [(a,b)] [(a,b)]


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

avgDeviation :: [Double] -> (Double,Double)
avgDeviation [] = (0/0,0)
avgDeviation xs = (a,sqrt (sum2 / len))
  where
    a         = sum/len
    (sum,len) = compute 0 0 xs
    sum2      = compute2 0 xs

    compute sum len []     = (sum,len)
    compute sum len (x:xs) = compute (sum+x) (len+1) xs

    compute2 sum []     = sum
    compute2 sum (x:xs) = compute2 (sum+square (x-a)) xs

    square x = x*x

density :: Ord b => [[b]] -> Double
density bs = fromIntegral (sum (map length bs)) / fromIntegral total
  where
    total = Set.size (Set.fromList (concat bs))

-- we want to count how many Data we have for each label. Thus we group it with 1 as 
-- singleton value and add 1 whenever we find another Datum with the same label       
counts :: Ord b => [b] -> Map.Map b Int 
counts = groupWith id (const (1::Int)) (const succ)

-- | How much information does this Attribute give us for the given Dataset.
-- It is defined as 
--
-- entropy(set) - sum p_i * entropy {dat_i | dat has value i for attribute a}
partition :: Ord b =>  [(a,b)]         -- ^ the data
          -> Attribute n a             -- ^ the attribute
          -> (Double,Partition n a b)  -- ^ the information and the partition
partition dataset attr =
  case attr of
    A  n project -> let p   = groupWith (project.fst) (:[]) (++) dataset
                        inf = total_ent - sum (map split_ent (Map.elems p))
                    in (inf,P n project p)
    AC n project -> let p   = groupWith (project.fst) (:[]) (++) dataset
                        ps  = split [] (Map.toList p)

                        split xs []            = []
                        split xs ((k,ys):rest) =
                          let pre  = ys++xs
                              post = concatMap snd rest
                          in PC n project k pre post : split pre rest

                    in (head.sortOn negatedInf) [(total_ent - (split_ent xs + split_ent ys),p) | p@(PC _ _ k xs ys) <- ps]
  where
    size = fromIntegral $ length dataset

    total_ent    = entropy (counts (map snd dataset))
    split_ent xs = (fromIntegral (length xs) / size) * entropy (counts (map snd xs))

-- | How much information does this Attribute give us for the given Dataset.
-- It is defined as 
--
-- entropy(set) - sum p_i * entropy {dat_i | dat has value i for attribute a}
partitionD :: Ord b =>  [(a,[b])]         -- ^ the data
           -> Attribute n a             -- ^ the attribute
           -> (Double,Partition n a [b])  -- ^ the information and the partition
partitionD dataset attr =
  case attr of
    A  n project -> let p   = groupWith (project.fst) (:[]) (++) dataset
                        inf | Map.null p = 0
                            | otherwise  = total_den - sum (map split_den (Map.elems p))
                    in (inf,P n project p)
    AC n project -> let p   = groupWith (project.fst) (:[]) (++) dataset
                        ps  = split [] (Map.toList p)

                        split xs []            = []
                        split xs ((k,ys):rest) =
                          let pre  = ys++xs
                              post = concatMap snd rest
                          in PC n project k pre post : split pre rest

                    in (head.sortOn negatedInf) [(total_den - (split_den xs + split_den ys),p) | p@(PC _ _ k xs ys) <- ps]
  where
    size = fromIntegral $ length dataset

    total_den    = density (map snd dataset)
    split_den xs = (fromIntegral (length xs) / size) * density (map snd xs)



partitionC :: [(a,Double)]                  -- ^ the data
           -> Attribute n a                 -- ^ the attribute
           -> (Double,Partition n a Double) -- ^ the information and the partition
partitionC dataset attr =
  case attr of
    A  n project -> let p   = groupWith (project.fst) (:[]) (++) dataset
                        inf = total_dev - sum (map split_dev (Map.elems p))
                    in (inf,P n project p)
    AC n project -> let p   = groupWith (project.fst) (:[]) (++) dataset
                        ps  = split [] (Map.toList p)

                        split xs []            = []
                        split xs ((k,ys):rest) =
                          let pre  = ys++xs
                              post = concatMap snd rest
                          in PC n project k pre post : split pre rest

                    in (head.sortOn negatedInf) [(total_dev - (split_dev xs + split_dev ys),p) | p@(PC _ _ k xs ys) <- ps]
  where
    size = fromIntegral $ length dataset

    total_dev    = snd (avgDeviation (map snd dataset))
    split_dev xs = (fromIntegral (length xs) / size) * snd (avgDeviation (map snd xs))


-- | Return the attribute which gives us greatest gain in information
bestAttribute :: Ord b => [(a,b)] -> [Attribute n a] -> (Double,Partition n a b)
bestAttribute dataset = head.sortOn negatedInf . map (partition dataset)

-- | Return the attribute which gives us greatest reduction in standard deviation
bestAttributeC :: [(a,Double)] -> [Attribute n a] -> (Double,Partition n a Double)
bestAttributeC dataset = head.sortOn negatedInf . map (partitionC dataset)

-- | Return the attribute which gives us greatest gain in information
bestAttributeD :: Ord b => [(a,[b])] -> [Attribute n a] -> (Double,Partition n a [b])
bestAttributeD dataset = head.sortOn negatedInf . map (partitionD dataset)

negatedInf (inf,_) = -inf

groupWith :: Ord k
          => (a -> k)      -- ^ how to extract a key
          -> (a -> v)      -- ^ how to make a value for the map
          -> (v -> v -> v) -- ^ how to fuse two values
          -> [a]           -- ^ the list we want to group
          -> Map.Map k v 
groupWith getKey singleton fuse =
    foldl (\m x -> Map.insertWith fuse (getKey x) (singleton x) m) Map.empty
