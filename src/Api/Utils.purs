module Api.Utils where

import Prelude

import Affjax.Web (request)
import Data.Either (Either(..), hush)
import Effect.Aff.Class (class MonadAff, liftAff)
import Api.Request (RequestOptions, defaultRequest)
import Data.Maybe (Maybe(..))
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Store (Action, Store)
import Halogen.Store.Monad (class MonadStore, getStore)
import Data.Codec.Argonaut (JsonCodec)
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
    pure case response of
        Left _ -> Nothing
        Right { status, body } -> if status == (StatusCode 200) then Just body else Nothing

decode :: forall m a. Monad m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode codec mbJson = pure do
            json <- mbJson
            decoded <- hush $ CA.decode codec json
            pure decoded