{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Person where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text

postPersonR :: Handler ()
postPersonR = do
    pers <- requireJsonBody :: Handler Person
    pid <- runDB $ insert pers
    sendResponse (object [pack "resp" .= pack ("CREATED " ++ (show $ fromSqlKey pid))])

getPeopleR :: Handler Html
getPeopleR = do
    pers <- runDB $ selectList [] [Asc PersonFirstname]
    sendResponse (object [pack "resp" .= toJSON pers])

getSecretaryR :: Handler Html
getSecretaryR = do
    boss <- runDB $ (rawSql "SELECT ??, ?? \
            \FROM depto INNER JOIN person \
            \ON depto.secretary=person.id" [])::Handler [(Entity Depto, Entity Person)]
    sendResponse (object [pack "resp" .= toJSON boss])                

    
    
    