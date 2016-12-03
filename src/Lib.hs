{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( Location (..)
    , Planning (..)
    )
    where

import Data.Text
import Data.Maybe
import Data.Time
import Data.Aeson as A
import Control.Lens hiding ((.=))
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as C8

data Location
   = Point { _lon :: Double, _lat :: Double}
    deriving (Show, Eq, Generic)
makeLenses ''Location

instance ToJSON Location where
  toJSON (Point lo la) = object
   [ "coordinates" .= [lo, la]
   , "type"        .= ("Point" :: Text)
   ]

instance FromJSON Location where
  parseJSON = withObject "Location" $ \o -> do
      [lo, la] <- o .: "coordinates"
      return $ Point lo la

data Planning
   = Planning
     { _pid           :: Text
     , _gssCode       :: Text
     , _ref           :: Text
     , _localId       :: Text
     , _numDwelings   :: Int
     , _address       :: Text
     , _title         :: Text
     , _location      :: Location
     , _extractUrl    :: Text
     , _planningType  :: Text
     , _decision      :: Text
     , _dateReceived  :: Maybe UTCTime
     , _decisionDate  :: Maybe UTCTime
     , _dateValid     :: Maybe UTCTime
     , _targetDate    :: Maybe UTCTime
     , _agentAddress  :: Text
     , _agentName     :: Text
     , _applicantName :: Text
     } deriving (Show, Eq, Generic)
makeLenses ''Planning

instance ToJSON Planning where
  toJSON Planning{..} = object
   [ "_id"            .= _pid
   , "gss_code"       .= _gssCode
   , "ref"            .= _ref
   , "local_id"       .= _localId
   , "num_dwellings"  .= _numDwelings
   , "address"        .= _address
   , "title"          .= _title
   , "location"       .= _location
   , "extract_url"    .= _extractUrl
   , "type"           .= _planningType
   , "decision"       .= _decision
   , "date_received"  .= _dateReceived
   , "decision_date"  .= _decisionDate
   , "date_valid"     .= _dateValid
   , "target_date"    .= _targetDate
   , "agent_address"  .= _agentAddress
   , "agent_name"     .= _agentName
   , "applicant_name" .= _applicantName
   ]

instance FromJSON Planning where
  parseJSON = withObject "Planning" $ \o ->
    Planning
      <$> o .: "_id"
      <*> o .: "gss_code"
      <*> o .: "ref"
      <*> o .: "local_id"
      <*> o .: "num_dwellings"
      <*> o .: "address"
      <*> o .: "title"
      <*> o .: "location"
      <*> o .: "extract_url"
      <*> o .: "type"
      <*> o .: "decision"
      <*> o .: "date_received"
      <*> o .: "decision_date"
      <*> o .: "date_valid"
      <*> o .: "target_date"
      <*> o .: "agent_address"
      <*> o .: "agent_name"
      <*> o .: "applicant_name"
