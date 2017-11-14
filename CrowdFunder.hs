import Prelude hiding (readFile, writeFile)

import Control.Arrow
import Control.Lens
import Data.Aeson
import Data.Maybe
import GHC.Generics
import Data.ByteString.Lazy.Char8
import System.IO hiding (hGetContents, writeFile)
import qualified System.IO.Strict as S
import Data.Time

import CrowdFunderTypes

initCrowdsale = Crowdsale {
  _beneficiary = 0,
  _fundingGoal = 100,
  _amountRaised = 20,
  _deadline = parseTimeOrError True defaultTimeLocale "%d %b %Y %l:%M %p" "26 Jan 2012 10:54 AM",
  _price = 15,
  _tokenReward = Token [],
  _funders = []
}

updateCrowdsale :: Message -> Crowdsale -> Crowdsale
updateCrowdsale message c = c & funders.~newFunders & amountRaised.~newAmountRaised 
  where
    (funder, amount) = (sender &&& value) message
    newFunders = c^.funders ++ [Funder funder amount]
    newAmountRaised = c^.amountRaised + amount

-- API

receiveMessage :: Message -> IO ()
receiveMessage message = do
  crowdsale <- readCrowdsale crowdsaleStateFilePath
  let newCrowdsale = updateCrowdsale message crowdsale
  writeCrowdsale crowdsaleStateFilePath newCrowdsale

-- IO
 
crowdsaleStateFilePath = "crowdsale.json"

currentCrowdsale :: IO Crowdsale
currentCrowdsale = readCrowdsale crowdsaleStateFilePath

readCrowdsale :: FilePath -> IO Crowdsale
readCrowdsale f = do
  file <- S.readFile f
  return ((fromMaybe initCrowdsale . decode . pack) file)

writeCrowdsale :: FilePath -> Crowdsale -> IO ()
writeCrowdsale f c = writeFile f (encode c)
