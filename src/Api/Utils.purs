module Api.Utils where

import Prelude

import Affjax.Web (request)
import Data.Bifunctor (rmap)
import Data.Either (hush)
import Effect.Aff.Class (class MonadAff, liftAff)
import Api.Request (RequestOptions)
import Data.Maybe (Maybe)
import Data.Argonaut.Core (Json)
import Api.Request (defaultRequest)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Codec.Argonaut as CA
import Store (Action, Store)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Effect.Console
import Effect.Class
import Affjax.StatusCode

mkRequest
    :: forall m
    . MonadAff m
    => MonadStore Action Store m
    => RequestOptions
    -> m (Maybe Json)
mkRequest opts = do
    { baseUrl } <- getStore
    response <- liftAff $ request $ defaultRequest baseUrl opts
    case response of
        Left err -> pure Nothing
        Right { status, body } -> pure $ if status == (StatusCode 200) then (Just body) else Nothing

decode :: forall m a. Monad m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = pure Nothing
decode codec (Just json) = case CA.decode codec json of
    Left err -> pure Nothing
    Right response -> pure (Just response)