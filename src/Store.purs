module Store where

import Prelude
import Data.Currency (Currency)
import Data.MoneyItem (MoneyItemWithId)
import Halogen.Store.Select (Selector)
import Halogen.Store.Select (selectEq)
import Data.Maybe (Maybe)
import Data.Maybe (Maybe(..))
import Api.Request (BaseURL)
import Network.RemoteData (RemoteData)
import Network.RemoteData (RemoteData(..))
import Halogen.Store.Select (select)

type Store =
    { baseUrl :: BaseURL
    , moneyItems :: RemoteData String (Array MoneyItemWithId)
    , currencies  :: RemoteData String (Array Currency)
    }

data Action = SetMoneyItems (RemoteData String (Array MoneyItemWithId))
    | SetCurrencies (RemoteData String (Array Currency)) -- add updates, removals
    | UpdateMoneyItem (MoneyItemWithId)

reduce :: Store -> Action -> Store
reduce store = case _ of
    SetMoneyItems items -> store { moneyItems = items }
    SetCurrencies items -> store { currencies = items }
    UpdateMoneyItem item -> store

isInitialized :: Store -> Boolean
isInitialized { moneyItems: Success _, currencies: Success _ } = true
isInitialized _ = false

selectIsInitialized :: Selector Store Boolean
selectIsInitialized = selectEq isInitialized

selectMoneyItems :: Selector Store (RemoteData String (Array MoneyItemWithId))
selectMoneyItems = selectEq _.moneyItems

selectCurrencies :: Selector Store (RemoteData String (Array Currency))
selectCurrencies = selectEq _.currencies