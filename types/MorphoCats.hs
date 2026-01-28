import Text.JSON
import Control.Monad
import System.FilePath
import System.Directory
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.List (sort,sortOn,intercalate,intersect,intercalate,mapAccumL)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Codec.Compression.GZip as GZip
import DecisionTree
import GF.Text.Pretty
import GF.Infra.Ident
import GF.Infra.Option
import GF.Grammar.Predef
import GF.Grammar.Grammar
import GF.Grammar.Printer
import Debug.Trace

data Tag
  = Tag 
      { tag   :: String
      , ident :: Ident
      , typ   :: Ident
      , order :: Int
      }

instance Eq Tag where
  t1 == t2 = order t1 == order t2

instance Ord Tag where
  compare t1 t2 = compare (order t1) (order t2)
  
instance Show Tag where
  show t = showIdent (ident t)

all_tags = flip (zipWith id) [0..] $
  [tag "positive"        "Posit"       "Degree"
  ,tag "comparative"     "Compar"      "Degree"
  ,tag "superlative"     "Superl"      "Degree"
  ,tag "infinitive"      "Inf"         "NonFinite"
  ,tag "past_participle" "PastPart"    "Participle"
  ,tag "participle"      "Part"        "Participle"
  ,tag "indefinite"      "Indef"       "Species"
  ,tag "definite"        "Def"         "Species"
  ,tag "unspecified"     "Unspecified" "Distance"
  ,tag "proximal"        "Proximal"    "Distance"
  ,tag "distal"          "Distal"      "Distance"
  ,tag "present"         "Present"     "Tense"
  ,tag "past"            "Past"        "Tense"
  ,tag "aorist"          "Aorist"      "Tense"
  ,tag "imperfect"       "Imperfect"   "Tense"
  ,tag "perfect"         "Perfect"     "Tense"
  ,tag "pluperfect"      "Pluperf"     "Tense"
  ,tag "past-perfect"    "PastPerfect" "Tense"
  ,tag "future"          "Future"      "Tense"
  ,tag "past-future"     "PastFuture"  "Tense"
  ,tag "future-perfect"  "FuturePerfect" "Tense"
  ,tag "imperfective"    "Imperf"      "Aspect"
  ,tag "perfective"      "Perf"        "Aspect"
  ,tag "imperative"      "Imperative"  "Mood"
  ,tag "conditional"     "Cond"        "Mood"
  ,tag "masculine"       "Masc"        "Gender"
  ,tag "feminine"        "Fem"         "Gender"
  ,tag "neuter"          "Neuter"      "Gender"
  ,tag "vocative"        "Voc"         "Case"
  ,tag "singular"        "Sg"          "Number"
  ,tag "plural"          "Pl"          "Number"
  ,tag "first-person"    "P1"          "Person"
  ,tag "second-person"   "P2"          "Person"
  ,tag "third-person"    "P3"          "Person"
  ,tag "count-form"      "count_form"  "CountForm"
  ]
  where
    tag tag ident typ = Tag tag (identS ident) (identS typ)

pos_tags =
  [("adj", "Adj")
  ,("adv", "Adverb")
  ,("conj","Conjunct")
  ,("det", "Detm")
  ,("intj", "Interjection")
  ,("name", "Name")
  ,("noun", "Noun")
  ,("prep", "Preposition")
  ,("verb", "Verb")
  ]


main = do
  dict <- readWiktionary "mk"
  let attrs    = (map toAttr . Map.toList . Map.fromListWith (++)) [(typ p,[p]) | p <- all_tags]
      tagsSets = toTagSets dict
  jments <- learnMorphoCats attrs Map.empty (Map.toList tagsSets)
  let mi = ModInfo {
             mtype = MTResource,
             mstatus = MSComplete,
             mflags = noOptions,
             mextend = [],
             mwith = Nothing,
             mopens = [],
             mexdeps = [],
             msrc = "",
             jments = jments
           }
  print (ppModule Unqualified (moduleNameS "ResX", mi))
  where
    toTagSets dict =
      Map.fromListWith combine [(pos, (Map.fromListWith (+) [(sort tags,1) | (form,tags) <- forms], [(word,tags,forms)])) | (word,pos,tags,forms) <- dict]
      where
        combine (tags1,words1) (tags2,words2) =
          (Map.unionWith (+) tags1 tags2, words1++words2)

    toAttr (typ,tags) = A (ParamName typ id) attrValue
      where
        attrValue :: [String] -> Maybe Tag
        attrValue str_tags =
          case [t | str_tag <- str_tags, t <- tags, tag t==str_tag] of
            (t:_) -> Just t
            []    -> Nothing

    inflectionTree forms dt = foldr (uncurry insert) (fmap (const Nothing) dt) forms
      where
        insert form tags (Leaf _ inf) = Leaf (Just form) inf
        insert form tags (Decision n proj dts) =
            Decision n proj (Map.adjust (insert form tags) (proj tags) dts)


readWiktionary lang = do
  createDirectoryIfMissing True ("data" </> lang)
  let fpath = "data" </> lang </> "wiktionary.data"
  exist <- doesFileExist fpath
  if exist
    then fmap (map toEntry . lines) $ readFile fpath
    else do putStrLn ("Extracting data from raw-wiktextract-data.jsonl.gz for "++lang++".")
            content <- fmap GZip.decompress (BS.readFile "../../rgl-learner/raw-wiktextract-data.jsonl.gz")
            let es = extractEntries (BS.lines content) []
            writeFile fpath (unlines [intercalate "\t" (word:pos:intercalate ";" tags:concatMap (\(form,tags) -> [form,intercalate ";" tags]) forms) | (word,pos,tags,forms) <- es])
            return es
  where
    extractEntries []           entries = entries
    extractEntries (line:lines) entries =
      case extract (BS.toString line) of
        Ok entry -> extractEntries lines (entry:entries)
        _        -> extractEntries lines entries
      where
        extract line = do
          o     <- decode line
          lang' <- valFromObj "lang_code" o
          guard (lang' == lang)
          word <- valFromObj "word" o
          pos  <- valFromObj "pos" o
          tags <- listFromObj "tags" o
          senses <- listFromObj "senses" o
          tags <- fmap (foldr (++) tags) (mapM (listFromObj "tags") senses)
          forms <- valFromObj "forms" o
          forms <- forM forms $ \o -> do
                     form <- valFromObj "form" o
                     tags <- valFromObj "tags" o
                     return (map wc form,tags)
          return (word,pos,tags,[(form,tags) | (form,tags) <- forms, form /= "-"])

        wc c | isSpace c = ' '
             | otherwise = c

    listFromObj field o =
      case lookup field (fromJSObject o) of
        Just v  -> readJSON v
        Nothing -> return []

    toEntry line =
      let (word:pos:tags:fs) = split '\t' line
      in (word,pos,split ';' tags,toForms fs)

    toForms []             = []
    toForms (form:tags:fs) =
      (form,split ';' tags) : toForms fs
    toForms xs = error (show xs)

    split sep []     = [[]]
    split sep (c:cs)
      | c == sep  = [] : split sep cs
      | otherwise = case split sep cs of
                      []     -> [[c]]
                      (x:xs) -> (c:x):xs


data ParamName a = ParamName Ident (a->Maybe Tag)

learnMorphoCats attrs jments []                            = return jments
learnMorphoCats attrs jments ((pos,(tagsSet0,words)):rest) = do
  case lookup pos pos_tags of
    Just pos -> do putStrLn ("=== "++pos)
                   let dts0 = iterateD [tags | (tags,count) <- Map.toList tagsSet0, count > min_count, tags /= ["comparative"] && tags /= ["superlative"]]
                       dts  = map (stack dts0) (iterate [tags | (tags,_) <- dts0])
                   forM_ dts $ \(tags,dt) -> do
                     print tags
                     putStrLn (drawDecisionTree (\(ParamName n _)->showIdent n) (\(ParamName n f)->maybe "?" (showIdent . ident) . f) show dt)
                   let rs      = map (\(tags,dt) -> (tags,toPreType dt)) dts
                       jments' = codeGen jments pos rs
                   learnMorphoCats attrs jments' rest
    Nothing  -> learnMorphoCats attrs jments rest
  where
    min_count = 5

    stack dts0 (tags0,dt) =
      let dt' = do tags <- dt
                   tags <- fromJust (lookup tags dts0)
                   return (fromJust (Map.lookup tags tagsSet0))
      in (tags0,dt')

    iterate []      = []
    iterate tagsSet =
      let (matched,dt) = build attrs [(tags,tags) | tags <- tagsSet]
          (tagsSet',dt') = prune [tags | tags <- tagsSet, isNothing (lookup tags matched)] dt
      in (foldl1 intersect dt',dt'):iterate tagsSet'
      where
        prune vs dt@(Leaf tags _) = (vs,dt)
        prune vs (Decision n@(ParamName name f) proj dts) =
          case fmap (\((k,dt),dts) -> ((f k,dt),dts)) (Map.minViewWithKey dts) of
            Just ((Nothing,dt),dts) -> let def_tag = proj (take 1 [tag t | t <- all_tags, typ t == name])
                                       in case Map.lookup def_tag dts of
                                            Just _  -> let (vs',dts') = mapAccumL prune vs dts
                                                       in unsingle (values dt++vs',Decision n proj dts')
                                            Nothing -> let (vs',dts') = mapAccumL prune vs (Map.insert def_tag dt dts)
                                                       in unsingle (vs',Decision n proj dts')
            _                       -> let (vs',dts') = mapAccumL prune vs dts
                                       in unsingle (vs',Decision n proj dts')

    iterateD []      = []
    iterateD tagsSet =
      let dt = buildD attrs [(tags,tags) | tags <- tagsSet]
          (tagsSet',dt') = prune [] dt
      in (foldl1 intersect dt',dt'):iterateD tagsSet'
      where
        prune vs dt@(Leaf tags _) = (vs,dt)
        prune vs (Decision n@(ParamName name f) proj dts) =
          case fmap (\((k,dt),dts) -> ((f k,dt),dts)) (Map.minViewWithKey dts) of
            Just ((Nothing,dt),dts) -> let def_tag = proj (take 1 [tag t | t <- all_tags, typ t == name])
                                       in case Map.lookup def_tag dts of
                                            Just _  -> let (vs',dts') = mapAccumL prune vs dts
                                                       in unsingle (values dt++vs',Decision n proj dts')
                                            Nothing -> let (vs',dts') = mapAccumL prune vs (Map.insert def_tag dt dts)
                                                       in unsingle (vs',Decision n proj dts')
            _                       -> let (vs',dts') = mapAccumL prune vs dts
                                       in unsingle (vs',Decision n proj dts')

    unsingle x@(vs,Decision n proj dts)
      | Map.size dts == 1 = (vs,head (Map.elems dts))
      | otherwise         = x


data PreParam = PPr Ident [(Tag,[PreParam])]
                deriving (Eq,Show)

toPreType (Leaf str_tags _) = []
toPreType (Decision (ParamName name f) _ dts) =
  case [(fromJust (f v),toPreType dt) | (v,dt) <- Map.toList dts] of
    ((k,pps):todo) -> let (cs,pps') = combine [(k,[])] todo pps
                      in PPr name cs : pps'
  where
    combine done []              pps2 = (reverse done,pps2)
    combine done ((k,pps1):todo) pps2 =
      let (pps1',pps2',pps) = intersect pps1 pps2
      in combine ((k,pps1'):map (\(k,pps2) -> (k,pps2++pps2')) done) todo pps

    intersect []                     []                     = ([],[],[])
    intersect []                     (PPr name2 cs2 : pps2) =
      let (pps1',pps2',pps) = intersect [] pps2
      in (pps1',(PPr name2 cs2):pps2',pps)
    intersect (PPr name1 cs1 : pps1) pps2                   =
      case common_with pps2 of
        Just pps2 -> let (pps1',pps2',pps) = intersect pps1 pps2
                     in (pps1',pps2',PPr name1 cs1 : pps)
        Nothing   -> let (pps1',pps2',pps) = intersect pps1 pps2
                     in (PPr name1 cs1 : pps1',pps2',pps)
      where
        common_with []                    = Nothing
        common_with (PPr name2 cs2 : pps2)
          | name1==name2 && cs1==cs2      = Just pps2
          | otherwise                     = case common_with pps2 of
                                              Just pps2 -> Just (PPr name2 cs2 : pps2)
                                              Nothing   -> Nothing



codeGen jments pos rs =
  let (jments',rs') = mapAccumL (\jments (tags,pps) -> let (jments',tys) = jmentsAndType jments pps in (jments',(tags,tys))) jments rs
      ty = RecType [(mkLabel tags i,[],foldr Table (Sort cStr) tys) | ((tags,tys),i) <- zip rs' [1..]]
  in Map.insert (identS pos) (ResOper Nothing (Just (noLoc ty))) jments'
  where
    mkLabel []       i
      | i == 1         = theLinLabel
      | otherwise      = linLabel i
    mkLabel str_tags i = ident2label (identS (map escape (intercalate "_" str_tags)))
      where
        escape '-' = '_'
        escape c   = c

    jmentsAndType jments []                  = (jments,[])
    jmentsAndType jments (PPr name ps : pps) =
      let (jments1,ps') = mapAccumL (\jments (p,pps) -> let (jments',tys) = jmentsAndType jments pps in (jments',(p,tys))) jments ps
          info = ResParam (Just (noLoc [(ident p, [(Explicit,identW,ty) | ty <- pps]) | (p,pps) <- ps'])) Nothing
          (name',jments2) = case [name | (name, info') <- Map.toList jments1, info==info'] of
                              []      -> let name'
                                               | Map.member name jments1 = identS (showIdent name ++ show (Map.size jments1))
                                               | otherwise               = name
                                         in (name',Map.insert name' info jments1)
                              name:_  -> (name,jments)
          (jments3,tys) = jmentsAndType jments2 pps
      in (jments3,Cn name' : tys)

