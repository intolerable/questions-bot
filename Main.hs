module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.DateTime (addMinutes, diffSeconds, getCurrentTime)
import Options.Applicative
import Reddit
import Reddit.Types.Post
import Reddit.Types.User
import Reddit.Types.Subreddit
import Text.Read
import System.Exit
import qualified Data.Text as Text

main :: IO ()
main = do
  execParser (info (helper <*> argsParser) fullDesc) >>= runBot

data Args =
  Args { username :: Username
       , password :: Text
       , previousPostID :: PostID
       , subreddit :: SubredditName
       , logFileName :: Maybe FilePath }
  deriving (Show, Read, Eq)

argsParser :: Parser Args
argsParser = Args <$> (Username <$> argument text (metavar "USERNAME"))
                  <*> argument text (metavar "PASSWORD")
                  <*> (PostID <$> argument text (metavar "POST"))
                  <*> (R <$> argument text (metavar "SUBREDDIT"))
                  <*> optional (option str (long "log-file" <> metavar "LOG"))
  where text = fmap Text.pack str

runBot :: Args -> IO ()
runBot a@(Args user pass post sub _log) = do
  res <- runRedditWithRateLimiting u pass $ do
    postInfo <- getPostInfo post
    time <- liftIO getCurrentTime
    let timeToPost = (week - 1) `addMinutes` created postInfo
    let timeUntilPost = timeToPost `diffSeconds` time
    liftIO $ threadDelay $ fromIntegral $ timeUntilPost * 1000 * 1000
    case genFromOld postInfo of
      Just (x, y) -> do
        pID <- submitSelfPost sub x y
        ensure $ do
          editPost pID (genFromNewAndOld sub pID postInfo)
          stickyPost pID
          setPostFlair sub pID "Question" "question"
        return pID
      _ -> liftIO $ do
        putStrLn "please give me a selfpost with content"
        exitFailure
  case res of
    Left err -> do
      print err
      threadDelay $ 5 * 1000 * 1000
      runBot a
    Right p -> do
      putStrLn "success, we did it!"
      runBot a { previousPostID = p }
  where Username u = user

ensure :: MonadIO m => RedditT m a -> RedditT m a
ensure a = do
  r <- nest a
  case r of
    Left (HTTPError _) -> do
      liftIO $ threadDelay $ 5 * 1000 * 1000
      ensure a
    Left e -> failWith e
    Right x -> return x

week :: Integer
week = 10080

hour :: Integer
hour = 60

twoMinute :: Integer
twoMinute = 2

genFromOld :: Post -> Maybe (Text, Text)
genFromOld oldPost = do
  oldContent <-
    case content oldPost of
      SelfPost c _ -> Just c
      _ -> Nothing
  nString <- getHidden "number" oldContent
  number <- readMaybe $ Text.unpack nString
  return ("The " <> ordinalize (succ number) <> " Weekly Stupid Questions Thread", oldContent)

ordinalize :: Int -> Text
ordinalize n =
  case n `rem` 100 of
    11 -> textShow n <> "th"
    12 -> textShow n <> "th"
    13 -> textShow n <> "th"
    _ ->
      case n `rem` 10 of
        1 -> textShow n <> "st"
        2 -> textShow n <> "nd"
        3 -> textShow n <> "rd"
        _ -> textShow n <> "th"
  where textShow = Text.pack . show

genFromNewAndOld :: SubredditName -> PostID -> Post -> Text
genFromNewAndOld (R sub) (PostID newPost) oldPost =
  oldContent & replaceSection (const $ "[last week's](" <> oldLink <> ")") "last_week"
             & replaceSection (const $ "[sort by new](" <> newLink <> ")") "this_week"
             & replaceHidden (const $ textShow (succ n)) "number"
  where newLink = "http://reddit.com/r/" <> sub <> "/comments/" <> newPost <> "/-/?sort=new"
        oldLink = "http://reddit.com/r/" <> sub <> "/comments/" <> oldPostID
        PostID oldPostID = postID oldPost
        SelfPost oldContent _ = content oldPost
        Just n = readMaybe . Text.unpack =<< getHidden "number" oldContent
        textShow = Text.pack . show :: Int -> Text

(&) :: a -> (a -> b) -> b
a & f = f $ a

replaceSection :: (Text -> Text) -> Text -> Text -> Text
replaceSection f del original =
  case Text.breakOnEnd (begin del) original of
    ("", _) -> original
    (h, rest) ->
      case Text.breakOn (end del) rest of
        (_, "") -> original
        (o, l) -> h <> f o <> l
  where begin d = "[](#begin " <> d <> ")"
        end d = "[](#end " <> d <> ")"

getHidden :: Text -> Text -> Maybe Text
getHidden section source =
  case Text.breakOnEnd (begin section) source of
    ("", _) -> Nothing
    (_, rest) ->
      case Text.breakOn ")" rest of
        (_, "") -> Nothing
        (o, _) -> Just o
  where begin d = "[](#hidden " <> d <> " "

replaceHidden :: (Text -> Text) -> Text -> Text -> Text
replaceHidden f del original =
  case Text.breakOnEnd (begin del) original of
    ("", _) -> original
    (h, rest) ->
      case Text.breakOn ")" rest of
        (_, "") -> original
        (o, l) -> h <> f o <> l
  where begin d = "[](#hidden " <> d <> " "
