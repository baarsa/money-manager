module Store where

import Prelude
import Data.Currency (Currency)
import Data.MoneyItem (MoneyItemWithId)
import Halogen.Store.Select (Selector, selectEq)
import Api.Request (BaseURL)
import Network.RemoteData (RemoteData(..))
import Data.Array

type Store =
    { baseUrl :: BaseURL
    , moneyItems :: RemoteData String (Array MoneyItemWithId)
    , currencies  :: RemoteData String (Array Currency)
    }

data Action = SetMoneyItems (RemoteData String (Array MoneyItemWithId))
    | SetCurrencies (RemoteData String (Array Currency))
    | UpdateMoneyItem (MoneyItemWithId)
    | DeleteMoneyItem Int
    | AddMoneyItem MoneyItemWithId

type WithId r = { id :: Int | r }

data AppDataState = NotReady | Ready (Array MoneyItemWithId)
derive instance Eq AppDataState

updateArray :: forall r. (WithId r) -> Array (WithId r) -> Array (WithId r)
updateArray newItem arr = map updateItem arr
    where updateItem :: (WithId r) -> (WithId r)
          updateItem oldItem = if oldItem.id == newItem.id then newItem else oldItem

reduce :: Store -> Action -> Store
reduce store = case _ of
    SetMoneyItems items -> store { moneyItems = items }
    SetCurrencies items -> store { currencies = items }
    UpdateMoneyItem item -> store { moneyItems = map (updateArray item) store.moneyItems }
    DeleteMoneyItem id -> store { moneyItems = map deleteItem store.moneyItems }
        where deleteItem :: Array MoneyItemWithId -> Array MoneyItemWithId
              deleteItem = filter $ (notEq id) <<< _.id
    AddMoneyItem item -> store { moneyItems = map addItem store.moneyItems }
        where addItem :: Array MoneyItemWithId -> Array MoneyItemWithId
              addItem arr = snoc arr item

selectAppDataState :: Selector Store AppDataState
selectAppDataState = selectEq getAppDataState
    where getAppDataState :: Store -> AppDataState
          getAppDataState { moneyItems: Success items, currencies: Success _ } = Ready items
          getAppDataState _ = NotReady

selectCurrencies :: Selector Store (RemoteData String (Array Currency))
selectCurrencies = selectEq _.currencies