{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module CrowdFunderTypes where

import Data.Aeson
import GHC.Generics
import Control.Lens
import Data.Time

type Address = Integer
type Amount = Integer

data Token = Token {
  coinBalanceOf :: [(Address, Amount)]
} deriving (Show, Generic)

instance FromJSON Token
instance ToJSON Token

data Funder = Funder {
  address :: Address,
  amount :: Amount
} deriving (Show, Generic)

instance FromJSON Funder
instance ToJSON Funder

data Crowdsale = Crowdsale { 
  _beneficiary :: Address,
  _fundingGoal :: Amount,
  _amountRaised :: Amount,
  _deadline :: UTCTime,
  _price :: Amount, -- Price of a token in Ether
  _tokenReward :: Token,
  _funders :: [Funder]
} deriving (Show, Generic)
makeLenses ''Crowdsale

instance FromJSON Crowdsale
instance ToJSON Crowdsale

data Message = Message {
  value :: Amount,
  sender :: Address
} deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message

