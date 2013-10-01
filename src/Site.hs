{-# LANGUAGE OverloadedStrings #-}

module Site where

import Application
import Templates (errorPage)

type Routes = [(B.ByteString, Handler App App ())]
