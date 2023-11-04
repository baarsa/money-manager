module Api.Request where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Argonaut.Core (Json)
import Api.Endpoint (Endpoint, endpointCodec)
import Affjax
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Data.Tuple (Tuple(..))
import Data.HTTP.Method (Method(..))
import Data.Either (Either(..))
import Routing.Duplex (print)

newtype BaseURL = BaseURL String

data RequestMethod
    = Get
    | Post (Maybe Json)
    | Put (Maybe Json)
    | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: BaseURL -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) { endpoint, method } =
     { method: Left requestMethod
      , url: baseUrl <> print endpointCodec endpoint
      , headers: []
      , content: map RB.json body
      , username: Nothing
      , password: Nothing
      , timeout: Nothing
      , withCredentials: false
      , responseFormat: RF.json
      }
      where
      Tuple requestMethod body = case method of
        Get -> Tuple GET Nothing
        Post b -> Tuple POST b
        Put b -> Tuple PUT b
        Delete -> Tuple DELETE Nothing