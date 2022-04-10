{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Timeline
  ( Instant,
    Timeline,
    new,
    add,
    newOrAdd,
    Data.Timeline.map,
    replaceLast,
    mapThenReplace,
    newOrReplace,
    Data.Timeline.join,
    replaceThenJoin,
    Data.Timeline.zip,
    concatWith,
    getForwardHistory,
    resolveJoin,
  )
where

import Data.List
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE

data Instant = Instant {unresolved :: Text, resolved :: Text}
  deriving (Show)

-- history goes backwards; the first element is the latest entry.
data Timeline = Timeline {history :: NonEmpty Instant, historyLength :: Int}
  deriving (Show)

new :: (Text, Text) -> Timeline
new (unres, res) = Timeline (one $ Instant unres res) 1

add :: Timeline -> (Text, Text) -> Timeline
add Timeline {..} (unresolved, resolved) =
  Timeline
    { history = Instant unresolved resolved <| history,
      historyLength = historyLength + 1
    }

newOrAdd :: Maybe Timeline -> (Text, Text) -> Timeline
newOrAdd Nothing = new
newOrAdd (Just tl) = add tl

map :: (Text -> Text) -> Timeline -> Timeline
map tFn tl@Timeline {history} =
  tl
    { history = fmap (\Instant {..} -> Instant (tFn unresolved) (tFn resolved)) history
    }

replaceLast :: Text -> Timeline -> Timeline
replaceLast newRes tl@Timeline {history = Instant unres _ :| hs} =
  tl {history = Instant unres newRes :| hs}

newOrReplace :: Text -> Maybe Timeline -> Timeline
newOrReplace t Nothing = new (t, t)
newOrReplace t (Just tl) = replaceLast t tl

mapThenReplace :: (Text -> Text) -> Text -> Timeline -> Timeline
mapThenReplace tFn newRes = replaceLast newRes . Data.Timeline.map tFn

join :: Timeline -> Timeline -> Timeline
join
  Timeline {history = aHist, historyLength = aLen}
  Timeline {history = bHist, historyLength = bLen} =
    Timeline {history = bHist <> aHist, historyLength = bLen + aLen}

resolveJoin :: NonEmpty Timeline -> Timeline
resolveJoin = foldr1 step
  where
    step :: Timeline -> Timeline -> Timeline
    step
      Timeline {history = bHist, historyLength = bLen}
      Timeline {history = Instant {unresolved = unA} :| aHist, historyLength = aLen} =
        Timeline {history = lastB :| initB ++ aHist, historyLength = bLen + aLen}
        where
          inst :| restB = NE.reverse bHist
          lastB :| initB = NE.reverse $ inst {unresolved = unA} :| restB

replaceThenJoin :: Timeline -> Text -> Timeline -> Timeline
replaceThenJoin tl1 t = Data.Timeline.join (replaceLast t tl1)

zip :: (Text -> Text -> Text) -> Timeline -> Timeline -> Timeline
zip
  tFn
  Timeline {history = aHist, historyLength = aLen}
  Timeline {history = bHist, historyLength = bLen}
    | difference < 0,
      Instant _ res :| _ <- aHist =
      Timeline
        { history = NE.zipWith liftedFn (pad (- difference) res aHist) bHist,
          historyLength = bLen
        }
    | Instant _ res :| _ <- bHist =
      Timeline
        { history = NE.zipWith liftedFn aHist (pad difference res bHist),
          historyLength = bLen
        }
    where
      difference = aLen - bLen
      liftedFn (Instant i1 o1) (Instant i2 o2) = Instant (tFn i1 i2) (tFn o1 o2)
      pad 0 _ ls = ls
      pad n v ls = (Instant v v :| replicate (n - 1) (Instant v v)) <> ls

concatWith :: (Text -> Text -> Text) -> NonEmpty Timeline -> Timeline
concatWith fn (tl :| tls) = foldl1' (flip $ Data.Timeline.zip fn) (tl : tls)

getForwardHistory :: Timeline -> [(Text, Text)]
getForwardHistory Timeline {history} = foldl' (\acc (Instant i o) -> (i, o) : acc) [] history
