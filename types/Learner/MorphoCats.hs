module Learner.MorphoCats(options,learn) where

import Text.JSON
import Control.Monad
import System.FilePath
import System.Directory
import System.Console.GetOpt
import Data.Char (isSpace, isDigit, toUpper)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.List (sort,sortOn,intercalate,intersect,intercalate,mapAccumL)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Codec.Compression.GZip as GZip
import Learner.RGL
import Learner.Config
import Learner.DecisionTree
import GF.Infra.Ident
import GF.Grammar.Predef
import GF.Grammar.Grammar

options =
  [ Option "v" [] (NoArg (\cfg->cfg{cfgVerbose=True})) "verbose output"
  , Option [] ["min-lexical-forms"] (ReqArg (\s cfg->cfg{cfgMinForms=read s}) "number") "the minimum number of forms per analysis"
  ]

learn cfg = do
  dict <- readWiktionary cfg
  let attrs    = (map toAttr . Map.toList . Map.fromListWith (++)) [(typ p,[p]) | p <- all_tags]
      tagsSets = toTagSets dict
  createDirectoryIfMissing True ("src" </> cfgLangName cfg)
  rgl <- readGrammar cfg
  rgl <- learnMorphoCats cfg attrs rgl{rglRes=(rglRes rgl){jments=Map.filter notParam (jments (rglRes rgl))}} (Map.toList tagsSets)
  rgl <- learnWorstCase cfg rgl (Map.toList tagsSets)
  writeGrammar cfg rgl
  where
    notParam (ResParam _ _) = False
    notParam _              = True

    toTagSets dict =
      Map.fromListWith combine [(pos, (Map.fromListWith (+) [(tags,1) | (tags,form) <- forms], [(word,tags,forms)])) | (word,pos,tags,forms) <- dict]
      where
        combine (tags1,words1) (tags2,words2) =
          (Map.unionWith (+) tags1 tags2, words1++words2)

    toAttr (typ,tags) = A (ParamName typ id) attrValue
      where
        attrValue :: [String] -> Maybe Tag
        attrValue str_tags =
          case [t | str_tag <- str_tags, t <- tags, wikt_tag t==str_tag] of
            (t:_) -> Just t
            []    -> Nothing

readWiktionary cfg = do
  let fdir  = "data" </> cfgLangName cfg
      fpath = fdir </> "wiktionary.data"
  createDirectoryIfMissing True fdir
  exist <- doesFileExist fpath
  if exist
    then withStatus ("Reading "++toTitle (cfgLangName cfg)++" lexicon from Wiktionary") $ do 
            es <- fmap (updateTags . map toEntry . lines) $ readFile fpath
            length es `seq` return es
    else withStatus ("Extracting data from raw-wiktextract-data.jsonl.gz for "++toTitle (cfgLangName cfg)) $ do
            content <- fmap GZip.decompress (BS.readFile "../../rgl-learner/raw-wiktextract-data.jsonl.gz")
            let es = updateTags (extractEntries (BS.lines content) [])
            writeFile fpath (unlines [intercalate "\t" (word:pos:intercalate ";" tags:concatMap (\(tags,form) -> [form,intercalate ";" tags]) forms) | (word,pos,tags,forms) <- es])
            return es
  where
    toTitle []     = []
    toTitle (c:cs) = toUpper c : cs

    extractEntries []           entries = entries
    extractEntries (line:lines) entries =
      case extract (BS.toString line) of
        Ok entry -> extractEntries lines (entry:entries)
        _        -> extractEntries lines entries
      where
        extract line = do
          o     <- decode line
          lang' <- valFromObj "lang_code" o
          guard (lang' == cfgIso2 cfg)
          word <- valFromObj "word" o
          pos  <- valFromObj "pos" o
          tags <- listFromObj "tags" o
          senses <- listFromObj "senses" o
          tags <- fmap (foldr (++) tags) (mapM (listFromObj "tags") senses)
          forms <- valFromObj "forms" o
          forms <- forM forms $ \o -> do
                     tags <- valFromObj "tags" o
                     form <- valFromObj "form" o
                     return (sort tags,map wc form)
          return (word,pos,sort tags,[(tags,form) | (tags,form) <- forms, form /= "-"])

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
      (split ';' tags,form) : toForms fs
    toForms xs = error (show xs)

    split sep []     = [[]]
    split sep (c:cs)
      | c == sep  = [] : split sep cs
      | otherwise = case split sep cs of
                      []     -> [[c]]
                      (x:xs) -> (c:x):xs

    updateTags es =
      [(word,pos,tags,[(sort tags,form) | (tags0,form) <- forms, tags <- cfgUpdTags cfg tags0]) | (word,pos,tags,forms) <- es]


data ParamName a = ParamName Ident (a->Maybe Tag)

learnMorphoCats cfg attrs rgl []                            = do
  let rgl' = rgl{rglRes=(rglRes rgl){jments=Map.mapWithKey fixParamName (jments (rglRes  rgl))}
                }
  return rgl'
  where
    fixParamName id info@(ResParam (Just (L loc ps)) extra) =
      case (reverse . takeWhile isDigit . reverse . showIdent) id of
        [] -> info
        x  -> ResParam (Just (L loc [(identS (showIdent p++x), pps) | (p,pps) <- ps])) extra
    fixParamName id info = info
learnMorphoCats cfg attrs rgl ((pos,(tagsSet0,words)):rest) = do
  case lookupPOS pos of
    Just pos -> do putStrLn ("=== "++posTag pos)
                   let dts0 = iterateD [tags | (tags,count) <- Map.toList tagsSet0, count > cfgMinForms cfg]
                       dts  = map (stack dts0) (iterate [tags | (tags,_) <- dts0])
                   when (cfgVerbose cfg) $
                     forM_ dts $ \(tags,dt) -> do
                       print tags
                       putStrLn (drawDecisionTree (\(ParamName n _)->showIdent n) (\(ParamName n f)->maybe "?" (showIdent . ident) . f) show dt)
                   let rs   = map (\(tags,dt) -> (tags,toPreType dt)) dts
                       rgl' = rgl{rglRes=(rglRes rgl){jments=genRes  (jments (rglRes  rgl)) pos rs}
                                 ,rglCat=(rglCat rgl){jments=genCat (jments (rglCat rgl)) pos}
                                 ,rglDict=(rglDict rgl){jments=genDict (jments (rglDict rgl)) pos words dts}
                                 ,rglDictAbs=(rglDictAbs rgl){jments=genDictAbs (jments (rglDictAbs rgl)) pos words dts}
                                 }
                   length dts `seq` learnMorphoCats cfg attrs rgl' rest
    Nothing  -> learnMorphoCats cfg attrs rgl rest
  where
    stack dts0 (tags0,dt) =
      let dt' = do tags <- dt
                   tags <- fromJust (lookup tags dts0)
                   return (tags,fromJust (Map.lookup tags tagsSet0))
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
            Just ((Nothing,dt),dts) -> let def_tag = proj (take 1 [wikt_tag t | t <- all_tags, typ t == name])
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
            Just ((Nothing,dt),dts) -> let def_tag = proj (take 1 [wikt_tag t | t <- all_tags, typ t == name])
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


learnWorstCase cfg rgl []                 = return rgl
learnWorstCase cfg rgl ((pos,(_,_)):rest) =
  case lookupPOS pos of
    Nothing  -> learnWorstCase cfg rgl rest
    Just pos -> case Map.lookup (posOper pos) (jments (rglRes rgl)) of
                  Nothing -> learnWorstCase cfg rgl rest
                  Just (ResOper Nothing (Just (L _ ty))) ->
                             let mkPOS = identS ('m':'k':showIdent (posOper pos))
                                 (t,t_ty) = type2term pos ty
                                 rgl'  = rgl{rglRes=(rglRes rgl){jments=Map.insert mkPOS (ResOper (Just (noLoc t_ty)) (Just (noLoc t))) (jments (rglRes rgl))}}
                             in learnWorstCase cfg rgl' rest
  where
    type2term pos ty =
      let (i,t0) = type2term 0 ty
          t      = foldr (Abs Explicit . f) t0 [0..i-1]
          t_ty   = foldr (\i -> Prod Explicit identW (Sort cStr)) (Cn (posOper pos)) [0..i-1]
      in (t,t_ty)
      where
        f i = identS ('f':show (i+1))

        type2term i (Sort s)
          | s == cStr  = (i+1,Vr (f i))
        type2term i (Table ty1 ty2) =
          let (i',cs) = mapAccumL (\i p -> let (i',t) = type2term i ty2 in (i',(p,t))) i (allParamPatts ty1)
          in (i',T (TTyped ty1) cs)
        type2term i (RecType rs) =
          let (i',rs') = mapAccumL (\i (l,_,ty) -> let (i',t) = type2term i ty in (i',(l,(Nothing,t)))) i rs
          in (i', R rs')

    allParamPatts (Cn id) =
      case Map.lookup id (jments (rglRes rgl)) of
        Just (ResParam (Just (L _ ps)) _) -> [PC p args | (p,ctxt) <- ps,
                                                          args <- sequence [allParamPatts ty | (_,_,ty) <- ctxt]]
        Nothing -> []

genRes jments pos rs =
  let (jments',rs') = mapAccumL (\jments (tags,pps) -> let (jments',tys) = jmentsAndType jments pps in (jments',(tags,tys))) jments rs
      ty = RecType [(mkLabel tags i,[],foldr Table (Sort cStr) tys) | ((tags,tys),i) <- zip rs' [1..]]
  in Map.insert (posOper pos) (ResOper Nothing (Just (noLoc ty))) jments'
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


genCat jments pos =
  Map.insert (posCat pos) (CncCat (Just (noLoc (Cn (posOper pos)))) Nothing Nothing Nothing Nothing) jments


genDict jments pos []                       dts = jments
genDict jments pos ((word,tags,forms):rest) dts
  | elem "form-of" tags = genDict jments pos rest dts
  | otherwise =
      let t = foldl (\t (_,dt) -> foldl (\t (tags,_) -> App t (maybe nonExist K (lookup tags forms))) t dt) (Vr (identS ('m':'k':showIdent (posOper pos)))) dts
          jments' = Map.insert (identS (word++"_"++showIdent (posCat pos))) (CncFun Nothing (Just (noLoc t)) Nothing Nothing) jments
      in genDict jments' pos rest dts
  where
    nonExist = QC (cPredef,cNonExist)


genDictAbs jments pos []                       dts = jments
genDictAbs jments pos ((word,tags,forms):rest) dts
  | elem "form-of" tags = genDictAbs jments pos rest dts
  | otherwise =
      let jments' = Map.insert (identS (word++"_"++showIdent (posCat pos))) (AbsFun (Just (noLoc (Cn (posCat pos)))) (Just 0) (Just []) Nothing) jments
      in genDictAbs jments' pos rest dts
