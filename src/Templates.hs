{-# LANGUAGE OverloadedStrings, RecordWildCards, BangPatterns #-}

module Templates(Page(..), errorPage) where

import Types

import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime(..))
import           Data.Time.Calendar (toGregorian)
import           Data.Time.Format (formatTime)
import           Prelude (String, ($), (++), (-), (==), return, Bool(..), Maybe(..), show)
import qualified Prelude as P
import           System.Locale (defaultTimeLocale)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes hiding (title, form, span)
import           Text.Pandoc (writeHtml, readMarkdown, def)
import           Text.Pandoc.Options (WriterOptions(..))

-- A data representation of a page to be rendered
data Page
  = Index (Maybe Text) Post
  | List  (Maybe Text) [PostRef]
  | Entry (Maybe Text) Post
  | Edit  Text RawPost
  | LoginForm (Maybe Text)
  | PasswdForm Text

instance ToMarkup Page where
  toMarkup (Index lg post@(Post { .. })) =
    page lg postTitle (postBody lg post)
  toMarkup (List lg ps) =
    page lg "Past Entries" (listBody ps)
  toMarkup (Entry lg post@(Post { .. })) =
    page lg postTitle (postBody lg post)
  toMarkup (Edit uname rawPost) =
    page (Just uname) "Create Post" (editForm uname rawPost)
  toMarkup (PasswdForm uname) =
    page (Just uname) "Change Password" (passwdForm uname)
  toMarkup (LoginForm lg) =
    page lg "Log In" loginForm

page :: Maybe Text -> Text -> Html -> Html
page isLoggedIn pgName pgContents = docTypeHtml $ do
  head $ do
    meta ! charset "utf-8"
    link ! rel "stylesheet" ! type_ "text/css" ! href "/static/main.css"
    script ! src "/static/main.js" $ return ()
    title (toHtml ("Baruk Khazâd: " `T.append` pgName))
  body ! id "bg" $ do
    userText
    div ! class_ "title" $ h1 $
      span ! class_ "bilingual" 
           ! dataAttribute "english" "Axes of the Dwarves! The Dwarves are Upon You!"
           ! dataAttribute "dwarvish" "Baruk Khazâd! Khazâd ai-Mênu!"
           $ ("Baruk Khazâd! Khazâd ai-Mênu!")
    div ! class_ "nav" $ titlebar isLoggedIn
    div ! class_ "main" $ pgContents
  where userText = case isLoggedIn of
                     Just user -> div ! class_ "username" $ do
                                    span ! class_ "msg" $ toMarkup ("Logged in as " <> user)
                                    " — "
                                    span ! class_ "lnk" $ do
                                      a ! href "/change" $ "Change My Password"
                                    " — "
                                    span ! class_ "lnk" $ do
                                      a ! href "/logout" $ "Log Out"
                     Nothing   -> return ()

errorPage :: Text -> Text -> Html
errorPage err desc = page Nothing ("Error: " <> err ) $ do
  div ! class_ "errmsg" $ toMarkup desc

passwdForm :: Text -> Html
passwdForm uname = do
  form ! name "passwd"
       ! action "/change"
       ! method "POST"
       ! enctype "application/x-www-form-urlencoded;charset=UTF-8" $ do
    toMarkup ("Old password for " <> uname <> ": ")
    br
    input ! type_ "password" ! name "oldpasswd"
    br
    "New password: "
    br
    input ! type_ "password" ! name "p1"
    br
    input ! type_ "password" ! name "p2"
    br
    input ! type_ "submit"

loginForm :: Html
loginForm = div ! class_ "login" $ do
  form ! name "login"
       ! action "/login"
       ! method "POST"
       ! enctype "application/x-www-form-urlencoded;charset=UTF-8" $ do
    input ! type_ "text" ! name "user"
    br
    input ! type_ "password" ! name "passwd"
    br
    input ! type_ "submit"

editForm :: Text -> RawPost -> Html
editForm uname (RawPost { .. }) = div ! class_ "edit" $ do
    form ! name "newpost"
         ! action "/"
         ! method "POST"
         ! enctype "application/x-www-form-urlencoded;charset=UTF-8" $ do
      let idVal = case rpId of
                    Nothing -> toValue ("none" :: String)
                    Just n  -> toValue n
      input ! type_ "hidden" ! name "id" ! value idVal
      "Title: "
      input ! type_ "text" ! name "title" ! value (toValue rpTitle)
      input ! type_ "hidden" ! name "author" ! value (toValue uname)
      br
      textarea ! cols "80" ! rows "40" ! name "contents" $ toHtml rpContents
      br
      input ! type_ "submit"

titlebar :: Maybe Text -> Html
titlebar user = P.mapM_ go links
  where go (lname, url) = a ! class_ "navitem" ! href url $ lname
        links = if isJust user then
                  [ ("Newest",  "/newest")
                  , ("Archive", "/archive")
                  , ("Create",  "/create")
                  , ("Oldest",  "/oldest")
                  ]
                else
                  [ ("Newest",  "/newest")
                  , ("Archive", "/archive")
                  , ("Log In",  "/auth")
                  , ("Oldest",  "/oldest")
                  ]

postLink :: PostRef -> Html
postLink (post@PostRef { .. }) =
  a ! href (toValue (urlFor post)) $ toHtml prName

postBody :: Maybe Text -> Post -> Html
postBody user (post@Post { .. }) = div ! class_ "post" $ do
  h2 (toHtml postTitle)
  div ! class_ "author" $ toHtml postAuthor

  let htmlOpts = def { writerHtml5 = True }
  let convPost = T.replace "\r\n" "\n" postContents
  writeHtml htmlOpts (readMarkdown def (T.unpack convPost))
  editLink user
  div ! class_ "new" $ maybeLink postNext "Newer"
  div ! class_ "old" $ maybeLink postPrev "Older"
    where maybeLink Nothing   _ = return ()
          maybeLink (Just pr) n = postLink (pr { prName = n })
          editLink (Just uname)
            | uname == postAuthor = a ! href editURL $ "Edit this post"
          editLink _ = return ()
          editURL = toValue (urlForPost post <> "/edit")

toMonth :: P.Int -> Html
toMonth n = span ! class_ "bilingual"
                 ! dataAttribute "english" ename
                 ! dataAttribute "dwarvish" dname
                 $ return ()
  where ename = [ "January", "February", "March"
                , "April",   "May",      "June"
                , "July",    "August",   "September"
                , "October", "November", "December"
                ] P.!! n
        dname = [ "Granite",   "Slate",     "Felsite"
                , "Hematite",  "Malachite", "Galena"
                , "Limestone", "Sandstone", "Timber"
                , "Moonstone", "Opal",      "Obsidian"
                ] P.!! n

formatDate :: UTCTime -> Html
formatDate t = do
  toMarkup (show day)
  " "
  toMonth (month - 1)
  ", "
  toMarkup (show year)
    where (year, month, day) = toGregorian (utctDay t)

listBody :: [PostRef] -> Html
listBody ps = div ! class_ "list" $ P.mapM_ go ps
  where go ref = do
          p $ do
            postLink ref
            span ! class_ "date" $ do
              " on "
              toMarkup (formatDate (prDate ref))
