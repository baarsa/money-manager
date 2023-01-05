module App where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as H
import Halogen.Subscription as HS
import Data.Maybe (Maybe(..))
import Halogen.Store.Connect (Connected)
import Halogen.Store.Connect (connect)
import Store (selectIsInitialized, Action, Store)
import Capability.MoneyItem (getMoneyItems, updateMoneyItem, deleteMoneyItem)
import Capability.Currency (getCurrencies)
import Data.MoneyItem (MoneyItem)
import AppM (AppM)
import Capability.MoneyItem (class ManageMoneyItems)
import Capability.Currency (class ManageCurrencies)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Monad (updateStore)
import Store (selectMoneyItems)
import Data.MoneyItem (MoneyItemWithId)
import Store (Action(..))
import Network.RemoteData (RemoteData(..), fromMaybe)
import Data.Lens.Fold (preview)
import Network.RemoteData (_Success)
import HTML.Utils (maybeElem)
import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)
import Type.Proxy (Proxy)
import Type.Proxy (Proxy(..))
import Component.MoneyItem as MoneyItem
import Component.Button as Button
import Data.Array (mapWithIndex)
import HTML.Utils (whenElem)

-- core component
-- here we have: list of money items, total amount and "move" modal,
--    plus button (to add new mitem), "pop-up notifications" (like portal)
-- it should also probably manage the state: fetch the items and currencies
-- and then set it into store (?)
-- or maybe it just manages the state locally and provides it to children

-- lifecycle: on init load currencies and articles
-- state: currencies, mitems, isCreating
-- action: initialize, set currencies, set mitems, add "new" (editing) item,
-- update item, delete item
-- input: no
-- render "loader" if isLoading, render array of mitems; if isCreating, render CreateMoneyItem;
-- render notifications (connected)

data WAction
    = Initialize
    | Receive (Connected (RemoteData String (Array MoneyItemWithId)) Unit)
    | HandleMoneyItemOutput { data :: MoneyItem.Output, id :: Int }
    | HandleNewItemOutput { data :: MoneyItem.Output }
    | HandleAddNewButtonOutput Button.Output

type State =
    { isCreating :: Boolean
    , isInitialized :: Boolean
    , moneyItems :: RemoteData String (Array MoneyItemWithId)
    }

type Slots =
    ( moneyItem :: forall query. H.Slot query MoneyItem.Output Int
    , button :: forall query. H.Slot query Button.Output Int
    )

_moneyItem = Proxy :: Proxy "moneyItem"

_button = Proxy :: Proxy "button"

app :: forall q o m. MonadStore Action Store m => ManageMoneyItems m => ManageCurrencies m => MonadEffect m => H.Component q Unit o m
app = connect selectMoneyItems $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }
    where
    initialState { context: moneyItems } =
        { isCreating: false
        , isInitialized: false
        , moneyItems }
    handleAction :: WAction -> H.HalogenM State WAction Slots o m Unit
    handleAction = case _ of
       Initialize -> do
            liftEffect $ log "fetching"
            moneyItems <- getMoneyItems unit --maybe get rid of parameter
            updateStore $ SetMoneyItems $ fromMaybe moneyItems
            currencies <- getCurrencies unit -- try do in parallel
            updateStore $ SetCurrencies $ fromMaybe currencies
       Receive { context: moneyItems } -> do
            H.modify_ _ { moneyItems = moneyItems }
       HandleMoneyItemOutput { data: MoneyItem.ConfirmedUpdate item, id } -> do
            let newItem = { id, name: item.name, currencyId: item.currencyId, amount: item.amount }
            result <- updateMoneyItem newItem
            updateStore $ UpdateMoneyItem newItem -- todo add failure handling
            pure unit
       HandleMoneyItemOutput { data: MoneyItem.ClickedDelete, id } -> do
            --send request, if successful remove element from array, else raise notification
            result <- deleteMoneyItem id
            updateStore $ DeleteMoneyItem id -- todo add failure handling
            pure unit
       HandleAddNewButtonOutput _ -> do
            H.modify_ _ { isCreating = true }
       _ -> do
            pure unit

    render :: State -> H.ComponentHTML WAction Slots m
    render { isCreating, isInitialized, moneyItems } =
        HH.div []
            [ renderMoneyItems moneyItems
            , whenElem isCreating newItem
            , whenElem (not isCreating) addNewButton
            ]
        where
            renderMoneyItem :: MoneyItemWithId -> _
            renderMoneyItem item = HH.slot _moneyItem item.id MoneyItem.moneyItem
                { name: item.name, currencyId: item.currencyId, amount: item.amount }
                (\outputData -> HandleMoneyItemOutput { data: outputData, id: item.id })
            renderMoneyItems :: RemoteData String (Array MoneyItemWithId) -> _
            renderMoneyItems = case _ of
                Success arr -> HH.div_ $ map renderMoneyItem arr
                _ -> HH.text "nothing"
            newItem _ = HH.slot _moneyItem 0 MoneyItem.moneyItem
                            { name: "", currencyId: 0, amount: 0 }-- select initial currency or empty?
                            (\outputData -> HandleNewItemOutput { data: outputData })
            addNewButton _ = HH.slot _button 0 Button.button { text: "Add new" } HandleAddNewButtonOutput