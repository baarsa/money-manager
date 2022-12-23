module Main where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Effect.Class (class MonadEffect)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Effect (Effect)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Effect.Random (random)
import Data.Maybe (Maybe(..), maybe)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Component.MoneyItem (moneyItem)
import Store (Store)
import App (app)
import AppM (runAppM)
import Api.Request (BaseURL(..))
import Network.RemoteData (RemoteData(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    initialStore :: Store
    initialStore = { baseUrl: BaseURL "http://localhost:3000", moneyItems: NotAsked, currencies: NotAsked }
  rootComponent <- runAppM initialStore app
  runUI rootComponent unit body
