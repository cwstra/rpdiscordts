{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throw)
import Data.Aeson
import Data.HistoryM
import Data.Resolved (Resolved, display)
import System.Random
import System.Timeout (timeout)
import Text.DiceParser.AST
import Text.DiceParser.Parser (runExprParser)
import qualified Web.Scotty as Scotty

liftEither :: Exception s => Either s b -> IO b
liftEither (Right b) = return b
liftEither (Left e) = throw e

data JSONResult = JSONFailure {errorMessage :: Text, input :: Text} | JSONSuccess {input :: Text, history :: [(Text, Text)], result :: Resolved}

instance ToJSON JSONResult where
  toJSON JSONFailure {errorMessage, input} = object ["status" .= ("Failure" :: Text), "errorMessage" .= errorMessage, "input" .= input]
  toJSON JSONSuccess {input, history, result} =
    object ["status" .= ("Success" :: Text), "input" .= input, "history" .= history, "result" .= (display result :: Text)]

  toEncoding JSONFailure {errorMessage, input} = pairs ("status" .= ("Failure" :: Text) <> "errorMessage" .= errorMessage <> "input" .= input)
  toEncoding JSONSuccess {input, history, result} =
    pairs ("status" .= ("Success" :: Text) <> "input" .= input <> "history" .= history <> "result" .= (display result :: Text))

toResult :: Text -> Either Text (Resolved, [(Text, Text)]) -> JSONResult
toResult input (Left errorMessage) = JSONFailure {errorMessage, input}
toResult input (Right (result, history)) = JSONSuccess {input, history, result}

main :: IO ()
main = Scotty.scotty 3000 $
  Scotty.get "/roll" $ do
    input <- Scotty.param "roll"
    seed <- Scotty.param "seed"
    jsonResult <- Scotty.liftAndCatchIO $
      timeout 10000000 $
        return $
          toResult input $ do
            ast <- runExprParser input
            let (resolved, (history, _)) = runHistory (mkStdGen seed) $ resolve ast
            result <- resolved
            return (result, history)
    maybe (Scotty.text "Computation timeout") Scotty.json jsonResult
