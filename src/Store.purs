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
import Data.Array (updateAt)
import Data.Array (findIndex)
import Data.Array (deleteAt)

type Store =
    { baseUrl :: BaseURL
    , moneyItems :: RemoteData String (Array MoneyItemWithId)
    , currencies  :: RemoteData String (Array Currency)
    }

data Action = SetMoneyItems (RemoteData String (Array MoneyItemWithId))
    | SetCurrencies (RemoteData String (Array Currency)) -- add removals
    | UpdateMoneyItem (MoneyItemWithId)
    | DeleteMoneyItem Int

getMbUpdatedArr arr item = do
    ind <- findIndex (\item2 -> item2.id == item.id) arr
    updateAt ind item arr

updateArr arr item = case mbUpdatedArr of
    Just updatedArr -> updatedArr
    _ -> arr
    where mbUpdatedArr = getMbUpdatedArr arr item

reduce :: Store -> Action -> Store
reduce store = case _ of
    SetMoneyItems items -> store { moneyItems = items }
    SetCurrencies items -> store { currencies = items }
    UpdateMoneyItem item -> case store.moneyItems of
        Success arr -> store { moneyItems = Success $ updateArr arr item }
        _ -> store
    DeleteMoneyItem id -> case store.moneyItems of
        Success arr -> store { moneyItems = Success $ nextArr}
            where
            nextArr = case deleteResult of
                Just arr2 -> arr2
                _ -> arr
            deleteResult = do
                ind <- findIndex (\item -> item.id == id) arr
                deleteAt ind arr
        _ -> store

isInitialized :: Store -> Boolean
isInitialized { moneyItems: Success _, currencies: Success _ } = true
isInitialized _ = false

selectIsInitialized :: Selector Store Boolean
selectIsInitialized = selectEq isInitialized

selectMoneyItems :: Selector Store (RemoteData String (Array MoneyItemWithId))
selectMoneyItems = selectEq _.moneyItems

selectCurrencies :: Selector Store (RemoteData String (Array Currency))
selectCurrencies = selectEq _.currencies