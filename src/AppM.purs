module AppM where

import Prelude
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Store (reduce, Store, Action)
import Safe.Coerce (coerce)
import Api.Utils (mkRequest)
import Api.Endpoint (Endpoint(..))
import Data.HTTP.Method (Method(..))
import Api.Utils (decode)
import Data.MoneyItem (moneyItemWithIdCodec)
import Data.MoneyItem (moneyItemCodec)
import Api.Request (RequestMethod(..))
import Data.Maybe (Maybe(..))
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CAR
import Data.MoneyItem (moneyItemsWithIdCodec)
import Data.Currency (currenciesCodec)
import Capability.MoneyItem (class ManageMoneyItems)
import Capability.Currency (class ManageCurrencies)
import Data.Either

newtype AppM a = AppM (StoreT Action Store Aff a)

runAppM :: forall q i o. Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

instance ManageMoneyItems AppM where
    getMoneyItems =
        map (decode moneyItemsWithIdCodec) $ mkRequest { endpoint: MoneyItems, method: Get }
    createMoneyItem moneyItem = do
        let
            codec = CAR.object "MoneyItem" { moneyItem: moneyItemCodec }
            method = Put $ Just $ Codec.encode codec { moneyItem }
        mbJson <- mkRequest { endpoint: MoneyItems, method }
        pure do
            decoded <- decode (CAR.object "MoneyItemWithId" { moneyItem: moneyItemWithIdCodec }) mbJson
            pure decoded.moneyItem
    updateMoneyItem moneyItem = do
        let
            codec = CAR.object "MoneyItem" { moneyItem: moneyItemWithIdCodec }
            method = Post $ Just $ Codec.encode codec { moneyItem }
        mbJson <- mkRequest { endpoint: MoneyItem moneyItem.id, method }
        pure do
            decoded <- decode (CAR.object "MoneyItem" { moneyItem: moneyItemWithIdCodec }) mbJson
            pure decoded.moneyItem
    deleteMoneyItem moneyItemId = do
        mbResponse <- mkRequest { endpoint: MoneyItem moneyItemId, method: Delete }
        pure $ map (const unit) $ note "Delete failed" mbResponse

instance ManageCurrencies AppM where
    getCurrencies =
        map (decode currenciesCodec) $ mkRequest { endpoint: Currencies, method: Get }