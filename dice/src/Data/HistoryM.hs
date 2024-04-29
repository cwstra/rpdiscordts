{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.HistoryM
  ( HistoryM,
    runHistory,
    showReturn,
    liftMaybe,
    Data.HistoryM.mapMaybe,
    --fork,
    --dualFork,
    withMap,
    withMapReplace,
    withZip,
    withZipReplace,
    withJoin,
    simpleRoll,
    injectRollResult,
  )
where

import Data.KeepDrop
import qualified Data.Simplified as Simplified
import qualified Data.Text as T
import Data.Timeline (Timeline)
import qualified Data.Timeline as TL
import Data.UserNumber
import System.Random
import qualified Text.Show

data HistoryZip = Zip (Text -> Text -> Text) [Timeline] | Join [Timeline]

instance Show HistoryZip where
  show (Zip _ tl) = "Zip fn " <> show tl
  show (Join tl) = "Join " <> show tl

mapToEntries :: ([Timeline] -> [Timeline]) -> HistoryZip -> HistoryZip
mapToEntries fn (Zip zFn tls) = Zip zFn $ fn tls
mapToEntries fn (Join tls) = Join $ fn tls

data HistoryContext g
  = HistoryContext {
      currentHistory :: [HistoryZip],
      randomGen :: g,
      maxDiceToDisplay :: Maybe Int
    }
type HistoryM g = ExceptT Text (State (HistoryContext g))

--getForwardHistory
runHistory :: Maybe Int -> g -> HistoryM g a -> (Either Text a, ([(Text, Text)], g))
runHistory maxDiceToDisplay g m = (res, (collapse zips, g'))
  where
    (res, ctx) = runState (runExceptT m) (HistoryContext {currentHistory = [], randomGen = g, maxDiceToDisplay = maxDiceToDisplay })
    zips = currentHistory ctx
    g' = randomGen ctx
    collapse [] = []
    collapse (Zip _ [] : zs) = collapse zs
    collapse (Join [] : zs) = collapse zs
    collapse [Zip tfn (tl : tls)] = TL.getForwardHistory $ TL.concatWith tfn $ tl :| tls
    collapse [Join (tl : tls)] = TL.getForwardHistory $ TL.resolveJoin $ tl :| tls
    collapse (Zip t1fn (tl1 : tl1s) : Zip t2fn tl2s : zs) = collapse $ Zip t2fn (TL.concatWith t1fn (tl1 :| tl1s) : tl2s) : zs
    collapse (Zip t1fn (tl1 : tl1s) : Join tl2s : zs) = collapse $ Join (TL.concatWith t1fn (tl1 :| tl1s) : tl2s) : zs
    collapse (Join (tl1 : tl1s) : Zip t2fn tl2s : zs) = collapse $ Zip t2fn (TL.resolveJoin (tl1 :| tl1s) : tl2s) : zs
    collapse (Join (tl1 : tl1s) : Join tl2s : zs) = collapse $ Join (TL.resolveJoin (tl1 :| tl1s) : tl2s) : zs

modifyHistory :: ([HistoryZip] -> [HistoryZip]) -> HistoryM g ()
modifyHistory fn = modify (\ctx -> ctx{currentHistory = fn (currentHistory ctx)})

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead fn (a : as) = fn a : as

modifyLatestZip = modifyHistory . mapHead

modifyLatestTimeline = modifyLatestZip . mapToEntries . mapHead

showReturn :: Show a => a -> HistoryM g a
showReturn a = do
  modifyHistory go
  return a
  where
    dis = show a
    go [] = [Zip (<>) [TL.new (dis, dis)]]
    go ls = mapHead mapper ls
    mapper (Zip fn es) = Zip fn $ TL.new (dis, dis) : es
    mapper (Join es) = Join $ TL.new (dis, dis) : es

{-
showReturn :: Show a => a -> HistoryM g a
showReturn a = do
  modifyLatestZip go
  return a
  where
    dis = show a
    go (Zip fn es) = Zip fn $ TL.new (dis, dis) : es
-}

liftMaybe :: Text -> Maybe a -> HistoryM g a
liftMaybe t = hoistEither . maybeToRight t

mapMaybe :: Text -> (a -> Maybe b) -> HistoryM g a -> HistoryM g b
mapMaybe t pred = (>>= (hoistEither . maybeToRight t . pred))

replace :: Show a => HistoryM g a -> HistoryM g a
replace ha = do
  a <- ha
  modifyLatestTimeline $ TL.replaceLast $ show a
  return a

withMap :: (Text -> Text) -> HistoryM g a -> HistoryM g a
withMap fn = (<* modifyLatestTimeline (TL.map fn))

withMapReplace :: Show a => (Text -> Text) -> HistoryM g a -> HistoryM g a
withMapReplace fn = replace . (<* modifyLatestTimeline (TL.map fn))

withZip :: (Text -> Text -> Text) -> HistoryM g a -> HistoryM g a
withZip fn ha = do
  modifyHistory (Zip fn [] :)
  a <- ha
  modifyHistory wrap
  return a
  where
    --wrap ((Zip fn1 (tl : tls)) : (Zip fn2 tl2) : zs) = Zip fn2 (TL.concatWith fn1 (tl :| tls) : tl2) : zs
    wrap ((Zip fn1 (tl : tls)) : (Zip fn2 tl2) : zs) = Zip fn2 (TL.concatWith fn1 (tl :| tls) : tl2) : zs
    wrap ((Zip fn1 (tl : tls)) : (Join tl2) : zs) = Join (TL.concatWith fn1 (tl :| tls) : tl2) : zs
    wrap [Zip fn1 (tl : tls)] = [Zip (<>) [TL.concatWith fn1 (tl :| tls)]]
    wrap _ = []

withJoin :: HistoryM g a -> HistoryM g a
withJoin ha = do
  modifyHistory (Join [] :)
  a <- ha
  modifyHistory wrap
  return a
  where
    --wrap ((Zip fn1 (tl : tls)) : (Zip fn2 tl2) : zs) = Zip fn2 (TL.concatWith fn1 (tl :| tls) : tl2) : zs
    wrap ((Join (tl : tls)) : (Zip fn2 tl2) : zs) = Zip fn2 (TL.resolveJoin (tl :| tls) : tl2) : zs
    wrap ((Join (tl : tls)) : (Join tl2) : zs) = Join (TL.resolveJoin (tl :| tls) : tl2) : zs
    wrap [Join (tl : tls)] = [Zip (<>) [TL.resolveJoin $ tl :| tls]]
    wrap _ = []

withZipReplace :: (a -> Text) -> (Text -> Text -> Text) -> HistoryM g a -> HistoryM g a
withZipReplace display fn ha = do
  res <- withZip fn ha
  modifyLatestTimeline $ TL.replaceLast $ display res
  return res

simpleRoll :: RandomGen g => Simplified.Dice -> HistoryM g (Text, GeneralNumber)
simpleRoll d = do
  ctx <- get
  let g = randomGen ctx
  let (terms, res, g') = Simplified.rollWrappedDice d g
  let termText = T.concat ["(", condShow terms (length terms) (maxDiceToDisplay ctx), ")"]
  put $ ctx{randomGen = g'}
  return (termText, res)
  where
    joinText = T.intercalate " + " . map (showKeptOrDropped show)
    condShow terms len maxDice
      | Just m <- maxDice, len > m =
        T.concat [joinText (take 2 terms), " + ...", show (len - 4), " other terms... + ", joinText (drop (len - 2) terms)]
      | otherwise = joinText terms

injectRollResult :: (Show a, RandomGen g) => (a -> Text) -> (Text, a) -> HistoryM g a
injectRollResult resShow (termText, res) = do
  modifyLatestTimeline $
    flip
      TL.add
      ( termText,
        resShow res
      )
  return res
