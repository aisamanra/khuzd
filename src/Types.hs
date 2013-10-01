{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}

module Types
  ( submitPost
  , newestPost
  , newestPostRef
  , oldestPostRef
  , listPosts
  , postByDateAndSlug

  , PostRef(..)
  , Post(..)
  , RawPost(..)

  , urlFor
  , urlForPost
  , toRaw
  ) where

import           Data.Char (isAlphaNum, toLower)
import           Data.Default (Default(..))
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime(utctDay), toGregorian, getCurrentTime)
import           Database.SQLite.Simple

singleResult :: IO [Only a] -> IO (Maybe a)
singleResult = fmap (fmap fromOnly . listToMaybe)

slugify :: Text -> Text
slugify = T.map conv
  where conv c | isAlphaNum c = toLower c
               | otherwise    = '-'

submitPost :: Text -> RawPost -> Connection -> IO Bool
submitPost uname rp c = case rpId rp of
  Just _  -> updatePost uname rp c
  Nothing -> insertPost uname rp c

updatePost :: Text -> RawPost -> Connection -> IO Bool
updatePost uname (RawPost { .. }) c = do
  if uname /= rpAuthor then return False else do
    let Just n = rpId
    execute c "UPDATE posts SET title = ?, author = ?, contents = ? WHERE id = ?"
      (rpTitle, rpAuthor, rpContents, n)
    execute c "UPDATE lookup SET slug = ? WHERE post_id = ?" (slugify rpTitle, n)
    return True

insertPost :: Text -> RawPost -> Connection -> IO Bool
insertPost uname (RawPost { .. }) c = do
  time <- getCurrentTime
  prev <- singleResult $ query_ c "SELECT id FROM posts ORDER BY time DESC LIMIT 1"
  execute c "INSERT INTO posts (title, contents, author, time, next, prev) VALUES (?,?,?,?,?,?)"
    (rpTitle, rpContents, uname, time, Nothing :: Maybe Int, prev :: Maybe Int)
  Just new <- singleResult $ query c "SELECT id FROM posts WHERE time = ?" (Only time)
  case prev of
    Just p -> execute c "UPDATE posts SET next = ? WHERE id = ?" (new :: Int, p)
    _ -> return ()
  let (year, month, _) = toGregorian (utctDay time)
  execute c "INSERT INTO lookup (year, month, time, slug, post_id) VALUES (?,?,?,?,?)"
    (year, month, time, slugify rpTitle, new)
  return True

newestPost :: Connection -> IO (Maybe Post)
newestPost c = do
  [Only n] <- query_ c "SELECT id FROM posts ORDER BY time DESC LIMIT 1"
  postById n c

newestPostRef :: Connection -> IO (Maybe PostRef)
newestPostRef c = do
  [Only n] <- query_ c "SELECT id FROM posts ORDER BY time DESC LIMIT 1"
  postRefById n c

oldestPostRef :: Connection -> IO (Maybe PostRef)
oldestPostRef c = do
  [Only n] <- query_ c "SELECT id FROM posts ORDER BY time ASC LIMIT 1"
  postRefById n c

listPosts :: Connection -> IO [PostRef]
listPosts c = do
  posts <- query_ c "SELECT year, month, slug, post_id FROM lookup ORDER BY time DESC"
  mapM go posts
    where go (prYear, prMonth, prSlug, n :: Int) =
             do [(prName, prDate)] <- query c "SELECT title, time FROM posts WHERE id = ?" (Only n)
                return (PostRef { .. })

postById :: Int -> Connection -> IO (Maybe Post) 
postById n c = do
  vals <- query c "SELECT id, title, contents, author, time, next, prev FROM posts WHERE id = ?" (Only n)
  case vals of
    [] -> return Nothing
    (postId, postTitle, postContents, postAuthor, postDate, nextId, prevId):_ -> do
      postNext <- maybe (return Nothing) (flip postRefById c) nextId
      postPrev <- maybe (return Nothing) (flip postRefById c) prevId
      return (Just (Post { .. }))

postByDateAndSlug :: Int -> Int -> String -> Connection -> IO (Maybe Post)
postByDateAndSlug year month slug c = do
  vals <- query c "SELECT post_id FROM lookup WHERE year = ? AND month = ? AND slug = ?"
            (year, month, slug)
  case vals of
    [] -> return Nothing
    (Only n:_) -> postById n c

postRefById :: Int -> Connection -> IO (Maybe PostRef)
postRefById n c = do
  vals <- query c "SELECT year, month, slug, post_id FROM lookup WHERE id = ?" (Only n)
  case vals of
    [] -> return Nothing
    (prYear, prMonth, prSlug, postId :: Int):_ -> do
      (prName, prDate):_ <- query c "SELECT title, time FROM posts WHERE id = ?" (Only postId)
      return (Just (PostRef { .. }))

-- Every post is referred to by a year, a month, a slug, and a name
data PostRef = PostRef
  { prYear  :: Int
  , prMonth :: Int
  , prSlug  :: Text
  , prName  :: Text
  , prDate  :: UTCTime
  } deriving Show

-- All the data for a particular post
data Post = Post
  { postId       :: Int
  , postDate     :: UTCTime
  , postTitle    :: Text
  , postContents :: Text
  , postAuthor   :: Text
  , postNext     :: Maybe PostRef
  , postPrev     :: Maybe PostRef
  } deriving Show

-- And all the data necessary to create a new post
data RawPost = RawPost
  { rpId       :: Maybe Int
  , rpTitle    :: Text
  , rpAuthor   :: Text
  , rpContents :: Text
  } deriving Show

instance Default RawPost where
  def = RawPost Nothing "" "" ""

urlForPost :: Post -> Text
urlForPost (Post { .. }) =
  let (year, month, _) = toGregorian (utctDay postDate) in
    T.concat [ "/", T.pack (show year)
             , "/", T.pack (show month)
             , "/", slugify postTitle
             ]

urlFor :: PostRef -> Text
urlFor (PostRef { .. }) =
  T.concat [ "/", T.pack (show prYear)
           , "/", T.pack (show prMonth)
           , "/", prSlug
           ]

toRaw :: Post -> RawPost
toRaw (Post { .. }) = RawPost
  { rpId       = Just postId
  , rpTitle    = postTitle
  , rpContents = postContents
  , rpAuthor   = postAuthor
  }
