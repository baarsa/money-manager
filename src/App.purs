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
import Capability.MoneyItem (getMoneyItems, updateMoneyItem, deleteMoneyItem, createMoneyItem)
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
import Effect.Aff.Class (class MonadAff)
import Type.Proxy (Proxy)
import Type.Proxy (Proxy(..))
import Component.MoneyItem as MoneyItem
import Component.CreateMoneyItem as CreateMoneyItem
import Component.Button as Button
import Component.Modal as Modal
import Component.Notifications as Notifications
import Data.Array (mapWithIndex)
import HTML.Utils (whenElem)
import Store as Store

data WAction
    = Initialize
    | Receive (Connected (RemoteData String (Array MoneyItemWithId)) Unit)
    | HandleMoneyItemUpdate { item :: MoneyItem.Input, id :: Int }
    | HandleNewItemCancel
    | HandleAddNewButtonOutput Button.Output
    | ShowConfirmDeleteModal Int
    | HandleDeleteModalClose Modal.Output Int
    | ShowConfirmAddModal CreateMoneyItem.State
    | HandleAddModalClose Modal.Output CreateMoneyItem.State

data ModalState a = Hidden | Visible a

type State =
    { isCreating :: Boolean
    , isInitialized :: Boolean
    , moneyItems :: RemoteData String (Array MoneyItemWithId)
    , confirmDeleteModal :: ModalState Int -- id of the item to delete on confirm
    , confirmAddModal :: ModalState CreateMoneyItem.State -- item data to add on confirm
    }

type Slots =
    ( moneyItem :: forall query. H.Slot query MoneyItem.Output Int
    , button :: forall query. H.Slot query Button.Output Int
    , createItem :: forall query. H.Slot query CreateMoneyItem.Output Int
    , modal :: forall query. H.Slot query Modal.Output Int
    , notifications :: H.Slot Notifications.Query Unit Unit
    )

_moneyItem = Proxy :: Proxy "moneyItem"

_createItem = Proxy :: Proxy "createItem"

_button = Proxy :: Proxy "button"

_modal = Proxy :: Proxy "modal"

_notifications = Proxy :: Proxy "notifications"

app :: forall q o m. MonadStore Store.Action Store.Store m => ManageMoneyItems m => ManageCurrencies m => MonadAff m => H.Component q Unit o m
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
        , moneyItems
        , confirmDeleteModal: Hidden
        , confirmAddModal: Hidden }
    handleAction :: WAction -> H.HalogenM State WAction Slots o m Unit
    handleAction = case _ of
       Initialize -> do
            moneyItems <- getMoneyItems unit --maybe get rid of parameter
            updateStore $ SetMoneyItems $ fromMaybe moneyItems
            currencies <- getCurrencies unit -- try do in parallel
            updateStore $ SetCurrencies $ fromMaybe currencies
       Receive { context: moneyItems } -> do
            H.modify_ _ { moneyItems = moneyItems }
       HandleMoneyItemUpdate { item, id } -> do
            let newItem = { id, name: item.name, currencyId: item.currencyId, amount: item.amount }
            result <- updateMoneyItem newItem
            updateStore $ UpdateMoneyItem newItem -- todo add failure handling
            pure unit
       ShowConfirmDeleteModal id -> do
            H.modify_ _ { confirmDeleteModal = Visible id }
       HandleDeleteModalClose closeResult id -> do
            case closeResult of
                Modal.Confirmed -> do
                    result <- deleteMoneyItem id
                    updateStore $ DeleteMoneyItem id -- todo add failure handling
                _ -> pure unit
            H.modify_ _ { confirmDeleteModal = Hidden }
       HandleAddNewButtonOutput _ -> do
            H.tell _notifications unit $ Notifications.PushNotification { level: Notifications.Warning, message: "Just saying" }
            H.modify_ _ { isCreating = true }
       ShowConfirmAddModal item -> do
           H.modify_ _ { confirmAddModal = Visible item }
       HandleAddModalClose closeResult item -> do
            case closeResult of
                Modal.Confirmed -> do
                    result <- createMoneyItem item
                    case result of
                        Just newItem -> updateStore $ AddMoneyItem newItem
                        _ -> pure unit -- show error
                    H.modify_ _ { isCreating = false }
                _ -> pure unit
            H.modify_ _ { confirmAddModal = Hidden }
       HandleNewItemCancel -> H.modify_ _ { isCreating = false }
       _ -> do
            pure unit

    render :: State -> H.ComponentHTML WAction Slots m
    render { isCreating, isInitialized, moneyItems, confirmDeleteModal, confirmAddModal } =
        HH.div []
            [ renderMoneyItems moneyItems
            , whenElem isCreating newItem
            , whenElem (not isCreating) addNewButton
            , renderConfirmDeleteModal
            , renderConfirmAddModal
            , HH.slot_ _notifications unit Notifications.notifications unit
            ]
        where
            renderMoneyItem :: MoneyItemWithId -> _
            renderMoneyItem item = HH.slot _moneyItem item.id MoneyItem.moneyItem
                { name: item.name, currencyId: item.currencyId, amount: item.amount }
                (\outputData -> case outputData of
                    MoneyItem.ConfirmedUpdate state -> HandleMoneyItemUpdate { item: state, id: item.id }
                    MoneyItem.ClickedDelete -> ShowConfirmDeleteModal item.id)
            renderMoneyItems :: RemoteData String (Array MoneyItemWithId) -> _
            renderMoneyItems = case _ of
                Success arr -> HH.div_ $ map renderMoneyItem arr
                _ -> HH.text "nothing"
            newItem _ = HH.slot _createItem 0 CreateMoneyItem.createMoneyItem unit (\output -> case output of
                CreateMoneyItem.Confirmed state -> ShowConfirmAddModal state
                CreateMoneyItem.Cancelled -> HandleNewItemCancel)
            addNewButton _ = HH.slot _button 0 Button.button { text: "Add new" } HandleAddNewButtonOutput
            renderConfirmDeleteModal = case confirmDeleteModal of
                Hidden -> HH.text ""
                Visible id -> HH.slot _modal 0 Modal.modal { title: "Confirm", text: "Are you sure you want to delte this item?" }
                    (\modalResult -> HandleDeleteModalClose modalResult id)
            renderConfirmAddModal = case confirmAddModal of
                Hidden -> HH.text ""
                Visible item -> HH.slot _modal 0 Modal.modal { title: "Confirm", text: "Are you sure you want to add this item?" }
                    (\modalResult -> HandleAddModalClose modalResult item)