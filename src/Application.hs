{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances #-}

module Application where

import Control.Lens
import Control.Monad.State (get)
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.SqliteSimple

data App = App
  { _sess :: Snaplet SessionManager
  , _db   :: Snaplet Sqlite
  , _auth :: Snaplet (AuthManager App)
  }

makeLenses ''App

type AppHandler = Handler App App

instance HasSqlite (Handler App App) where
  getSqliteState = with db get
