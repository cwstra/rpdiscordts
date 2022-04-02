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
    roll,
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

data HistoryZip = Zip (Text -> Text -> Text) [Timeline]

instance Show HistoryZip where
  show (Zip _ tl) = "Zip fn " <> show tl

mapToEntries :: ([Timeline] -> [Timeline]) -> HistoryZip -> HistoryZip
mapToEntries fn (Zip zFn tls) = Zip zFn $ fn tls

type HistoryM g = ExceptT Text (State ([HistoryZip], g))

--getForwardHistory
runHistory :: g -> HistoryM g a -> (Either Text a, ([(Text, Text)], g))
runHistory g m = (res, (collapse zips, g'))
  where
    (res, (zips, g')) = runState (runExceptT m) ([], g)
    collapse [] = []
    collapse (Zip _ [] : zs) = collapse zs
    collapse [Zip tfn (tl : tls)] = TL.getForwardHistory $ TL.concatWith tfn $ tl :| tls
    collapse (Zip t1fn (tl1 : tl1s) : Zip t2fn tl2s : zs) = collapse $ Zip t2fn (TL.concatWith t1fn (tl1 :| tl1s) : tl2s) : zs

modifyHistory :: ([HistoryZip] -> [HistoryZip]) -> HistoryM g ()
modifyHistory = modify . first

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
    go ls = mapHead (\(Zip fn es) -> Zip fn $ TL.new (dis, dis) : es) ls

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
    wrap [Zip fn1 (tl : tls)] = [Zip (\a b -> a <> "" <> b) [TL.concatWith fn1 (tl :| tls)]]
    wrap _ = []

withZipReplace :: (a -> Text) -> (Text -> Text -> Text) -> HistoryM g a -> HistoryM g a
withZipReplace display fn ha = do
  res <- withZip fn ha
  modifyLatestTimeline $ TL.replaceLast $ display res
  return res

roll :: RandomGen g => Simplified.Dice -> HistoryM g GeneralNumber
roll d = do
  g <- gets snd
  let (terms, res, g') = Simplified.rollWrappedDice d g
  let termText = T.concat ["(", T.intercalate " + " $ map (showKeptOrDropped show) terms, ")"]
  modifyLatestTimeline $ flip TL.add (termText, numShow res)
  modify $ second $ const g'
  return res
