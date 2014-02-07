{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Application
import           Templates
import           Types

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Char8 (ByteString, readInt, unpack)
import           Data.Default (def)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
import           Text.Blaze (toMarkup)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)


basicError :: Response
basicError = setResponseCode 400 emptyResponse

main :: IO ()
main = do
  (_, s, _) <- runSnaplet Nothing app
  quickHttpServe s

handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin _ = writeBuilder (renderHtmlBuilder (errorPage err msg))
  where err = "Authentication failed!"
        msg = "Unknown user or password"

handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = loginUser "user" "passwd" Nothing (\_ -> handleLogin err) (redirect "/")
  where err = Just "Unknown user or password"

handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

app :: SnapletInit App App
app = makeSnaplet "khuzd" "Strike the earth!" Nothing $ do
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  d <- nestSnaplet "db" db sqliteInit
  a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
  addRoutes routes
  return $ App s d a

routes :: [(ByteString, AppHandler ())]
routes = [ ("/",                          method POST doAddPost <|>
                                            (doIndex  >>= doPage))
         , ("/auth",                      doLoginForm >>= doPage)
         , ("/archive",                   doArchive   >>= doPage)
         , ("/create",                    doCreate    >>= doPage)
         , ("/:year/:month/:slug",        doPost      >>= doPage)
         , ("/:year/:month/:slug/edit",   editPost    >>= doPage)
         , ("/:year/:month/:slug/delete", deletePost  >>= doPage)
         , ("/login",                     with auth handleLoginSubmit)
         , ("/logout",                    with auth handleLogout)
         , ("/newest",                    doNewestRedirect)
         , ("/oldest",                    doOldestRedirect)
         , ("/change",                    method POST doPasswdChange <|>
                                            (doPasswdForm >>= doPage))
         , ("/static",                    serveDirectory "static")
         , ("/newuser",                   method POST doNewUser)
         ]

doPasswdForm :: AppHandler Page
doPasswdForm = do
  user <- fmap userLogin `fmap` with auth currentUser
  case user of
    Just u  -> return (PasswdForm u)
    Nothing -> redirect "/"

doPasswdChange :: AppHandler ()
doPasswdChange = do
  Just op <- getParam "oldpasswd"
  Just p1 <- getParam "p1"
  Just p2 <- getParam "p2"
  user <- with auth currentUser
  case user of
    Nothing -> finishWith basicError
    Just u  ->
      case authenticatePassword u (ClearText op) of
        Just _  -> finishWith basicError
        Nothing ->
          if p1 /= p2
            then finishWith basicError
            else do a <- liftIO (setPassword u p1)
                    with auth $ saveUser a
                    redirect "/"
          

doLoginForm :: AppHandler Page
doLoginForm = do
  user <- fmap userLogin `fmap` with auth currentUser
  return (LoginForm user)

doNewUser :: AppHandler ()
doNewUser = do
  Just user   <- getParam "user"
  Just passwd <- getParam "pass"
  with auth $ createUser (T.pack (unpack user)) passwd
  redirect "/"

doCreate :: AppHandler Page
doCreate = do
  user <- fmap userLogin `fmap` with auth currentUser
  case user of
    Just u -> return (Edit u def)
    _      -> redirect "/"

doIndex :: AppHandler Page
doIndex = do
  newest <- withSqlite newestPost
  user <- fmap userLogin `fmap` with auth currentUser
  case newest of
    Just pg -> return (Index user pg)
    Nothing -> finishWith basicError

doAddPost :: AppHandler ()
doAddPost = do
  allowed <- with auth isLoggedIn
  if allowed then do
      rp <- getRawPost
      Just uname <- fmap userLogin `fmap` with auth currentUser
      success <- withSqlite (submitPost uname rp)
      if success
        then redirect "/"
        else finishWith basicError
    else
      redirect "/"

doArchive :: AppHandler Page
doArchive = do
  user  <- fmap userLogin `fmap` with auth currentUser
  posts <- withSqlite listPosts
  return (List user posts)

doOldestRedirect :: AppHandler ()
doOldestRedirect = do
  oldest <- withSqlite oldestPostRef
  case oldest of
    Nothing -> finishWith basicError
    Just pr -> do
      let url = urlFor pr
      redirect (encodeUtf8 url)

doNewestRedirect :: AppHandler ()
doNewestRedirect = do
  newest <- withSqlite newestPostRef
  case newest of
    Nothing -> finishWith basicError
    Just pr -> do
      let url = urlFor pr
      redirect (encodeUtf8 url)

toInt :: ByteString -> AppHandler Int
toInt bs = case readInt bs of
             Just (n, "") -> return n
             _ -> finishWith basicError

getPost :: AppHandler (Maybe Post)
getPost = do
  Just year  <- getParam "year"
  Just month <- getParam "month"
  Just slug  <- getParam "slug"
  year'  <- toInt year
  month' <- toInt month
  let slug'  = unpack slug
  withSqlite (postByDateAndSlug year' month' slug')

getRawPost :: AppHandler RawPost
getRawPost = do
  Just rpTitle    <- fmap decodeUtf8 `fmap` getParam "title"
  Just rpAuthor   <- fmap decodeUtf8 `fmap` getParam "author"
  Just rpContents <- fmap decodeUtf8 `fmap` getParam "contents"
  idNum <- getParam "id"
  let rpId = case idNum of
               Just "none" -> Nothing
               _ -> maybe Nothing (fmap fst . readInt) idNum
  return RawPost { .. }

doPost :: AppHandler Page
doPost = do
  post <- getPost
  user  <- fmap userLogin `fmap` with auth currentUser
  case post of
    Just pg -> return (Entry user pg)
    Nothing -> finishWith basicError

editPost :: AppHandler Page
editPost = do
  post <- getPost
  user  <- fmap userLogin `fmap` with auth currentUser
  case (post, user) of
    (Just pg, Just u)
      | u == postAuthor pg -> return (Edit u (toRaw pg))
    _                      -> finishWith basicError

deletePost :: AppHandler Page
deletePost = do
  post <- getPost
  user <- fmap usrLogin `fmap` with auth currentUser
  case (post, user) of
    (Just pg, Just u)
      | u == postAuthor pg -> return (Delete u (toRaw pg))
    _                      -> finishWith basicError

doPage :: Page -> AppHandler ()
doPage = writeBuilder . renderHtmlBuilder . toMarkup
