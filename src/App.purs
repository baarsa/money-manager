module App where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as H
import Halogen.Subscription as HS
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Connect (Connected, connect)
import Capability.MoneyItem (class ManageMoneyItems, getMoneyItems, updateMoneyItem, deleteMoneyItem, createMoneyItem)
import Capability.Currency (class ManageCurrencies, getCurrencies)
import Component.Button as Button
import Component.CreateMoneyItem as CreateMoneyItem
import Component.Notifications as Notifications
import Component.MoneyItem as MoneyItem
import Component.Modal as Modal
import Component.Spinner as Spinner
import Data.Array (concat)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.MoneyItem (MoneyItemWithId)
import Network.RemoteData (RemoteData(..), fromMaybe)
import HTML.Utils (elemsByCondition, cssClass)
import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff)
import Type.Proxy (Proxy(..))
import Store (Action(..), AppDataState(Ready), Store, selectAppDataState)
import Control.Applicative (when)

data AppAction
    = Initialize
    | Receive (Connected AppDataState Unit)
    | HandleMoneyItemUpdate { item :: MoneyItem.Input, id :: Int }
    | HandleNewItemCancel
    | HandleAddNewButtonOutput Button.Output
    | ShowConfirmDeleteModal Int
    | HandleDeleteModalClose Modal.Output Int
    | ShowConfirmAddModal CreateMoneyItem.State
    | HandleAddModalClose Modal.Output CreateMoneyItem.State

data ModalState a = Hidden | Visible a -- a - тип данных, необходимых при обработке подтверждения действия

type State =
    { isCreating :: Boolean
    , appDataState :: AppDataState
    , confirmDeleteModal :: ModalState Int -- id элемента, для которого запрошено удаление
    , confirmAddModal :: ModalState CreateMoneyItem.State -- данные элемента, который нужно добавить
    }

type Slots =
    ( moneyItem :: H.Slot MoneyItem.Query MoneyItem.Output Int
    , button :: forall query. H.Slot query Button.Output Int
    , createItem :: forall query. H.Slot query CreateMoneyItem.Output Int
    , modal :: forall query. H.Slot query Modal.Output Int
    , notifications :: H.Slot Notifications.Query Unit Unit
    , spinner :: forall query. H.Slot query Unit Unit
    )

_moneyItem = Proxy :: Proxy "moneyItem"

_createItem = Proxy :: Proxy "createItem"

_button = Proxy :: Proxy "button"

_modal = Proxy :: Proxy "modal"

_notifications = Proxy :: Proxy "notifications"

_spinner = Proxy :: Proxy "spinner"

app :: forall q o m. MonadStore Action Store m => ManageMoneyItems m => ManageCurrencies m => MonadAff m => H.Component q Unit o m
app = connect selectAppDataState $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }
    where
    initialState { context: appDataState } =
        { isCreating: false
        , appDataState
        , confirmDeleteModal: Hidden
        , confirmAddModal: Hidden }
    handleAction :: AppAction -> H.HalogenM State AppAction Slots o m Unit
    handleAction = case _ of
       Initialize -> do
            updateStore $ SetMoneyItems Loading
            updateStore $ SetCurrencies Loading
            moneyItems <- getMoneyItems
            updateStore $ SetMoneyItems $ fromMaybe moneyItems -- incorrect, because Nothing -> NotAsked
            currencies <- getCurrencies -- try do in parallel
            updateStore $ SetCurrencies $ fromMaybe currencies
       Receive { context: appDataState } -> do
            H.modify_ _ { appDataState = appDataState }
       HandleMoneyItemUpdate { item, id } -> do
            let newItem = { id, name: item.name, currencyId: item.currencyId, amount: item.amount }
            result <- updateMoneyItem newItem
            case result of
                Just updatedItem -> do
                    updateStore $ UpdateMoneyItem updatedItem
                    showSuccessNotification "Item updated successfully"
                Nothing -> do
                    showErrorNotification "Failed to update item"
                    H.tell _moneyItem id $ MoneyItem.ResetState
            pure unit
       ShowConfirmDeleteModal id -> do
            H.modify_ _ { confirmDeleteModal = Visible id }
       HandleDeleteModalClose closeResult id -> do
            when (closeResult == Modal.Confirmed) do
              result <- deleteMoneyItem id
              case result of
                  Left message -> showErrorNotification message
                  _ -> updateStore $ DeleteMoneyItem id
            H.modify_ _ { confirmDeleteModal = Hidden }
       HandleAddNewButtonOutput _ -> do
            H.modify_ _ { isCreating = true }
       ShowConfirmAddModal item -> do
           H.modify_ _ { confirmAddModal = Visible item }
       HandleAddModalClose closeResult item -> do
            when (closeResult == Modal.Confirmed) do
                result <- createMoneyItem item
                case result of
                    Just newItem -> updateStore $ AddMoneyItem newItem
                    _ -> showErrorNotification "Failed to create item"
                H.modify_ _ { isCreating = false }
            H.modify_ _ { confirmAddModal = Hidden }
       HandleNewItemCancel -> H.modify_ _ { isCreating = false }
    showErrorNotification text = H.tell _notifications unit $ Notifications.PushNotification { level: Notifications.Error, message: text }
    showSuccessNotification text = H.tell _notifications unit $ Notifications.PushNotification { level: Notifications.Success, message: text }
    render :: State -> H.ComponentHTML AppAction Slots m
    render { isCreating, appDataState, confirmDeleteModal, confirmAddModal } =
        HH.div [ cssClass "app-container" ]
            [ HH.div [ cssClass "items-container" ] $ concat
                [ renderContent appDataState
                , [elemsByCondition isCreating newItem addNewButton]
                ]
            , renderConfirmDeleteModal
            , renderConfirmAddModal
            , HH.slot_ _notifications unit Notifications.notifications unit
            ]
        where
            renderMoneyItem :: MoneyItemWithId -> _
            renderMoneyItem item = HH.slot _moneyItem item.id MoneyItem.moneyItem
                { name: item.name, currencyId: item.currencyId, amount: item.amount }
                \outputData -> case outputData of
                    MoneyItem.ConfirmedUpdate state -> HandleMoneyItemUpdate { item: state, id: item.id }
                    MoneyItem.ClickedDelete -> ShowConfirmDeleteModal item.id
            renderContent :: AppDataState -> _
            renderContent = case _ of
                Ready arr -> map renderMoneyItem arr
                _ -> [HH.slot_ _spinner unit Spinner.spinner unit]
            newItem = HH.slot _createItem 0 CreateMoneyItem.createMoneyItem unit
                \output -> case output of
                    CreateMoneyItem.Confirmed state -> ShowConfirmAddModal state
                    CreateMoneyItem.Cancelled -> HandleNewItemCancel
            addNewButton = HH.slot _button 0 Button.button { text: "Add new" } HandleAddNewButtonOutput
            renderConfirmDeleteModal = case confirmDeleteModal of
                Hidden -> HH.text ""
                Visible id -> HH.slot _modal 0 Modal.modal { title: "Confirm", text: "Are you sure you want to delete this item?" }
                    \modalResult -> HandleDeleteModalClose modalResult id
            renderConfirmAddModal = case confirmAddModal of
                Hidden -> HH.text ""
                Visible item -> HH.slot _modal 0 Modal.modal { title: "Confirm", text: "Are you sure you want to add this item?" }
                    \modalResult -> HandleAddModalClose modalResult item