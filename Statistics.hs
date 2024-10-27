#!/usr/bin/env cabal
{- cabal:
build-depends:
  base, binary, bytestring, data-default < 0.8, github,
  optparse-applicative, text, time, vector
default-language: GHC2021
ghc-options: -Wall -Wno-type-defaults
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Exception
import Data.Binary
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (forM_)
import Data.List (sort)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Tuple
import Data.Vector qualified as V
import GitHub qualified as GH
import Options.Applicative
import System.IO

data Config
  = Offline FilePath
  | Online (Maybe B.ByteString) (Maybe FilePath)

configParser :: Parser Config
configParser = do
  let
    parseUsername =
      optional $
        strOption $
          long "user"
            <> metavar "USERNAME"
            <> help "GitHub username to bypass anonymous API rate limit"
    parseCacheFile =
      optional $
        strOption $
          long "cache"
            <> metavar "FILE"
            <> help "File to save cache to"
    parseOffline =
      strOption $
        long "offline"
          <> metavar "FILE"
          <> help "Work offline using previously saved cache"

  Offline <$> parseOffline <|> Online <$> parseUsername <*> parseCacheFile

getPassword :: IO B.ByteString
getPassword = do
  hFlush stdout
  pass <-
    bracket_
      (hSetEcho stdin False)
      (hSetEcho stdin True)
      B.getLine
  putChar '\n'
  pure pass

getBasicAuth :: B.ByteString -> IO GH.Auth
getBasicAuth username = do
  putStr "Launching missiles...\nEnter password to abort: "
  password <- getPassword
  pure $ GH.BasicAuth username password

getGithubIssues :: Config -> IO (V.Vector GH.Issue)
getGithubIssues (Offline cacheFile) =
  decode <$> BL.readFile cacheFile
getGithubIssues (Online mUsername mCacheFile) = do
  let githubOrg = "haskell"
      githubRepo = "core-libraries-committee"
  am <- traverse getBasicAuth mUsername
  response <- case am of
    Nothing -> GH.github () GH.issuesForRepoR githubOrg githubRepo GH.stateAll GH.FetchAll
    Just ba -> GH.github ba GH.issuesForRepoR githubOrg githubRepo GH.stateAll GH.FetchAll
  issues <- case response of
    Left err -> error $ show err
    Right is -> pure is
  case mCacheFile of
    Nothing -> pure ()
    Just cacheFile -> BL.writeFile cacheFile (encode issues)
  pure issues

data Issue = Issue
  { issNumber :: !Int
  , issTitle :: !Text
  , issCreatedAt :: !UTCTime
  , issClosedAt :: !(Maybe UTCTime)
  , issLabels :: ![Text]
  , issComments :: !Int
  }
  deriving (Eq, Ord, Show)

githubIssueToIssue :: GH.Issue -> Issue
githubIssueToIssue GH.Issue {..} = Issue {..}
  where
    issNumber = GH.unIssueNumber issueNumber
    issTitle = issueTitle
    issCreatedAt = issueCreatedAt
    issClosedAt = issueClosedAt
    issLabels = map (GH.untagName . GH.labelName) $ V.toList issueLabels
    issComments = issueComments

computeLifeTimeInDays :: Issue -> Maybe Double
computeLifeTimeInDays Issue {..} = case issClosedAt of
  Nothing -> Nothing
  Just t -> Just $ realToFrac (diffUTCTime t issCreatedAt) / 86400

computeDaysSinceCreation :: UTCTime -> Issue -> Double
computeDaysSinceCreation currTime Issue {..} =
  realToFrac (diffUTCTime currTime issCreatedAt) / 86400

isApproved :: Issue -> Bool
isApproved Issue {..} = "approved" `elem` issLabels

isDeclined :: Issue -> Bool
isDeclined Issue {..} = "declined" `elem` issLabels

isMeta :: Issue -> Bool
isMeta Issue {..} = "meta" `elem` issLabels

isBase :: Int -> Issue -> Bool
isBase n Issue {..} = ("base-4." <> T.pack (show n)) `elem` issLabels

data Stat = Stat
  { statMinIssue :: Issue
  , statMinMetric :: Double
  , statMinIssue2 :: Issue
  , statMinMetric2 :: Double
  , statMed :: Int
  , statAvg :: Int
  , statMaxIssue2 :: Issue
  , statMaxMetric2 :: Double
  , statMaxIssue :: Issue
  , statMaxMetric :: Double
  }
  deriving (Show)

collectStat :: (Issue -> Maybe Double) -> [Issue] -> Stat
collectStat f is = Stat {..}
  where
    (statMinIssue, statMinMetric) = getMin is
    (statMinIssue2, statMinMetric2) = getMin $ filter (/= statMinIssue) is

    statMed = round $ median $ mapMaybe f is
    statAvg = round $ average $ mapMaybe f is

    (statMaxIssue, statMaxMetric) = getMax is
    (statMaxIssue2, statMaxMetric2) = getMax $ filter (/= statMaxIssue) is

    getExtremum g = swap . g . mapMaybe (\x -> (,x) <$> f x)
    getMin = getExtremum minimum
    getMax = getExtremum maximum

    median xs = sort xs !! (length xs `quot` 2)
    average xs = sum xs / fromIntegral (length xs)

main :: IO ()
main = do
  cnf <-
    execParser $
      info
        (configParser <**> helper)
        (fullDesc <> header "Collect statistics for CLC proposals")
  issues <- getGithubIssues cnf

  let proposals =
        filter (not . isMeta) . map githubIssueToIssue $
          filter (isNothing . GH.issuePullRequest) $
            V.toList issues
      approvedProposals = filter isApproved proposals
      declinedProposals = filter isDeclined proposals

  putStrLn $ "Total number of CLC proposals: " ++ show (length proposals)
  let startTime = read "2021-10-23 00:00:00 UTC"
  currTime <- getCurrentTime
  putStrLn $ "Rate of proposals:  " ++ show (round (fromIntegral (length proposals) * 86400 * 365.25 / 12 / realToFrac (diffUTCTime currTime startTime))) ++ " per month"
  putStrLn $ "Approved proposals: " ++ show (length approvedProposals)
  putStrLn $ "Declined proposals: " ++ show (length declinedProposals)
  putStrLn ""

  let allLifeTime = collectStat computeLifeTimeInDays proposals
      approvedLifeTime = collectStat computeLifeTimeInDays approvedProposals
  putStrLn $ "Median  time from creation to decision: " ++ show (statMed allLifeTime) ++ " days"
  putStrLn $ "Average time from creation to decision: " ++ show (statAvg allLifeTime) ++ " days"
  putStrLn $ "Median  time from creation to approval: " ++ show (statMed approvedLifeTime) ++ " days"
  putStrLn $ "Average time from creation to approval: " ++ show (statAvg approvedLifeTime) ++ " days"
  putStrLn $ "Fastest approval:\n\t" ++ show (round (statMinMetric approvedLifeTime * 24)) ++ " hours for " ++ show (issTitle (statMinIssue approvedLifeTime))
  putStrLn $ "2nd fastest approval:\n\t" ++ show (round (statMinMetric2 approvedLifeTime * 24)) ++ " hours for " ++ show (issTitle (statMinIssue2 approvedLifeTime))
  putStrLn $ "2nd slowest approval:\n\t" ++ show (round (statMaxMetric2 approvedLifeTime)) ++ " days for " ++ show (issTitle (statMaxIssue2 approvedLifeTime))
  putStrLn $ "Slowest approval:\n\t" ++ show (round (statMaxMetric approvedLifeTime)) ++ " days for " ++ show (issTitle (statMaxIssue approvedLifeTime))
  putStrLn ""

  let allComments = collectStat (Just . fromIntegral . issComments) proposals
      approvedComments = collectStat (Just . fromIntegral . issComments) approvedProposals
  putStrLn $ "Total activity: " ++ show (sum (map issComments proposals)) ++ " comments"
  putStrLn $ "Median  activity per proposal:          " ++ show (statMed allComments) ++ " comments"
  putStrLn $ "Average activity per proposal:          " ++ show (statAvg allComments) ++ " comments"
  putStrLn $ "Median  activity per approved proposal: " ++ show (statMed approvedComments) ++ " comments"
  putStrLn $ "Average activity per approved proposal: " ++ show (statAvg approvedComments) ++ " comments"
  putStrLn $ "Least active approved proposal:\n\t" ++ show (round (statMinMetric approvedComments)) ++ " comment for " ++ show (issTitle (statMinIssue approvedComments))
  putStrLn $ "2nd least active approved proposal:\n\t" ++ show (round (statMinMetric2 approvedComments)) ++ " comments for " ++ show (issTitle (statMinIssue2 approvedComments))
  putStrLn $ "2nd most active:\n\t" ++ show (round (statMaxMetric2 allComments)) ++ " comments for " ++ show (issTitle (statMaxIssue2 allComments))
  putStrLn $ "Most active:\n\t" ++ show (round (statMaxMetric allComments)) ++ " comments for " ++ show (issTitle (statMaxIssue allComments))
  putStrLn ""

  forM_ [16 .. 21] $ \n ->
    putStrLn $ "Released in base-4." ++ show n ++ ": " ++ show (length (filter (isBase n) approvedProposals))
  putStrLn ""

  let openProposals = filter (isNothing . issClosedAt) proposals
  putStrLn $ "Open proposals: " ++ show (length openProposals)
  let openLifeTime = collectStat (Just . computeDaysSinceCreation currTime) openProposals
  putStrLn $ "Median  age for open proposals: " ++ show (statMed openLifeTime) ++ " days"
  putStrLn $ "Average age for open proposals: " ++ show (statAvg openLifeTime) ++ " days"
  putStrLn $ "Newest open proposal:\n\t" ++ show (round (statMinMetric openLifeTime)) ++ " days for " ++ show (issTitle (statMinIssue openLifeTime))
  putStrLn $ "Oldest open proposal:\n\t" ++ show (round (statMaxMetric openLifeTime)) ++ " days for " ++ show (issTitle (statMaxIssue openLifeTime))
