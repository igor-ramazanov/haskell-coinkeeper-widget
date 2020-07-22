{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( run,
  )
where

import           Control.Exception         (throw)
import           Control.Monad
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Time                 (UTCTime, toGregorian, utctDay)
import qualified Data.Vector               as V
import           GHC.Generics              (Generic)
import           Network.HTTP.Req
import           System.Environment        (lookupEnv)

newtype Budget = Budget Double deriving (Show)

newtype AuthCookie = AuthCookie T.Text deriving (Show)

newtype UserId = UserId T.Text deriving (Show)

newtype Diff = Diff Double deriving (Show)

newtype TotalAvailableToday = TotalAvailableToday Int deriving (Show)

data Config = Config
  { budget :: Maybe Budget
  , ac     :: AuthCookie
  , uid    :: UserId
  }
  deriving (Show, Generic)

readConfig :: MaybeT IO Config
readConfig = do
  authCookie <- fmap (AuthCookie . T.pack) $ MaybeT $ lookupEnv "COINKEEPER_AUTH_COOKIE"
  userId <- fmap (UserId . T.pack) $ MaybeT $ lookupEnv "COINKEEPER_USER_ID"
  maybeBudget <- lift (fmap (Budget . read) <$> lookupEnv "COINKEEPER_BUDGET")
  return $ Config maybeBudget authCookie userId

data Category = Category
  { deleted               :: Bool
  , periodizedLimitAmount :: Double
  }
  deriving (Show, Generic)

data PingResponse = PingResponse
  { totalNumberOfDaysInPeriod   :: Int
  , currentNumberOfDaysInPeriod :: Int
  , expenseSpentBalance         :: Double
  }
  deriving (Show, Generic)

data Transaction = Transaction
  { sourceType        :: Int
  , destinationType   :: Int
  , destinationAmount :: Double
  , dateTimestampISO  :: UTCTime
  }
  deriving (Show, Generic)

dayOfMonth :: UTCTime -> Int
dayOfMonth time = n
  where
    day = utctDay time
    (_, _, n) = toGregorian day

instance FromJSON Category

instance FromJSON PingResponse

instance FromJSON Transaction

(^?) :: Value -> T.Text -> Maybe Value
(^?) (Object obj) k = HM.lookup k obj
(^?) _ _            = Nothing

ix :: Value -> Int -> Maybe Value
ix (Array arr) i = arr V.!? i
ix _ _           = Nothing

extractEntity :: Value -> Int -> Maybe Value
extractEntity v n = do
  d <- v ^? "data"
  items <- d ^? "items"
  item <- ix items n
  entityJsonValue <- item ^? "entityJson"
  entityJsonText <- fmap TE.encodeUtf8 $ case entityJsonValue of
    Data.Aeson.String t -> Just t
    _                   -> Nothing
  decodeStrict' entityJsonText

parsePingResponse :: Value -> Maybe PingResponse
parsePingResponse v = do
  entityJson <- extractEntity v 5
  parseMaybe parseJSON entityJson

parseCategories :: Value -> Maybe [Category]
parseCategories v = do
  entityJson <- extractEntity v 2
  parseMaybe parseJSON entityJson

parseTransactions :: Value -> Maybe [Transaction]
parseTransactions v = do
  values <-
    fmap
      ( \case
          (Array arr) -> V.toList arr
          _ -> []
      )
      $ v ^? "transactions"
  traverse (parseMaybe parseJSON) values

ping :: Config -> IO Value
ping (Config _ (AuthCookie authCookie) _) = runReq defaultHttpConfig $ do
  let payload = object ["items" .= V.fromList (Prelude.map (\(i :: Int) -> object ["key" .= i, "entityJson" .= Null]) [0 .. 6])]
  r <- req POST (https "coinkeeper.me" /: "Exchange" /: "Ping") (ReqBodyJson payload) jsonResponse (header "Cookie" $ TE.encodeUtf8 authCookie)
  let response :: Value = responseBody r
  return response

transactions :: Config -> IO Value
transactions (Config _ (AuthCookie authCookie) (UserId userId)) = runReq defaultHttpConfig $ do
  let payload :: Value = object ["userid" .= userId, "skip" .= (0 :: Int), "take" .= (40 :: Int), "categoryIds" .= emptyArray, "tagIds" .= emptyArray, "period" .= emptyObject]
  r <- req POST (https "coinkeeper.me" /: "api" /: "transaction" /: "get") (ReqBodyJson payload) jsonResponse (header "Cookie" $ TE.encodeUtf8 authCookie)
  let response :: Value = responseBody r
  return response

calcTotalAvailableToday :: PingResponse -> Budget -> Diff -> TotalAvailableToday
calcTotalAvailableToday (PingResponse totalDays currentDay expenses) (Budget b) (Diff d) = TotalAvailableToday $ div (round leftTotal) leftDays
  where
    leftTotal = b - expenses + d
    leftDays = totalDays - currentDay + 1

calcCoinKeeperBudget :: [Category] -> Budget
calcCoinKeeperBudget cs = Budget $ foldl (\acc c -> if not $ deleted c then acc + periodizedLimitAmount c else acc) 0 cs

calcDiff :: PingResponse -> [Transaction] -> Diff
calcDiff pr tx = Diff $ foldl (\acc t -> acc + if dayOfMonth (dateTimestampISO t) == currentNumberOfDaysInPeriod pr then destinationAmount t else 0) 0 tx

calcAvailableToday :: Config -> Value -> Value -> Maybe Int
calcAvailableToday c prV txV = do
  pr <- parsePingResponse prV
  tx <- parseTransactions txV
  cs <- parseCategories prV
  let coinKeeperBudget = calcCoinKeeperBudget cs
  let fallbackedBudget = fromMaybe coinKeeperBudget $ budget c
  let diff@(Diff d) = calcDiff pr tx
  let (TotalAvailableToday totalToday) = calcTotalAvailableToday pr fallbackedBudget diff
  let availableNow = totalToday - round d
  return availableNow

run' :: MaybeT IO Int
run' = do
  c <- readConfig
  pr <- lift $ ping c
  tx <- lift $ transactions c
  MaybeT $ pure $ calcAvailableToday c pr tx

run :: IO ()
run = runMaybeT run' >>= maybe (throw $ userError "Some error happened") print
