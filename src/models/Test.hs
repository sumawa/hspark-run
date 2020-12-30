{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test where

import Database.Persist
import Database.Persist.Postgresql
import qualified Database.Persist.TH as PTH
import Data.Time (UTCTime)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
User
    firstname String
    lastname String
    email String
    UniqueUser email
    deriving Show Eq

Link
    url String
    title String
    deriving Show

Category
    category String
    deriving Show

UserLink
    userId UserId
    linkId LinkId
    created UTCTime default=CURRENT_TIMESTAMP
    deriving Show

UserLinkCategory
    userLinkId UserLinkId
    categoryId CategoryId
    deriving Show
|]

type Firstname = String
type Lastname = String
type Email = String
