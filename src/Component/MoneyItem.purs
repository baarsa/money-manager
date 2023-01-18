module Component.MoneyItem where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Component.StringInput as StringInput
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Effect.Aff (forkAff)
import Control.Monad.Rec.Class (forever)
import Effect.Aff (delay)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Data.Int (fromString)
import Data.MoneyItem (MoneyItem)
import Data.Currency (Currency)
import Data.MoneyItem (MoneyItem)
import Data.Number.Format (toString)
import Data.Int (toStringAs)
import Data.Int (decimal)
import Data.String.CodeUnits (fromCharArray)
import Component.Button as Button
import Component.CurrencyControl as CurrencyControl
import Component.NumberInput as NumberInput
import ComponentMode
import Store as Store
import Halogen.Store.Monad (class MonadStore)
import Prim.Row
import HTML.Utils (whenElem)
import Component.EditMoneyItem as EditMoneyItem
import Component.EditMoneyItem

-- state: item data without id; mode: view | edit;
-- output: on save (moneyItem), on delete ()

type Input = { name :: String
                     , currencyId :: Int
                     , amount :: Int }

data Query a = ResetState a

data Output = ConfirmedUpdate Input | ClickedDelete

type State = {
    item :: Input,
    initialItemState :: Input,
    mode :: ComponentMode
}

data Action = Initialize
    | Receive Input
    | HandleEditButton Button.Output
    | HandleConfirmButton Button.Output
    | HandleDeleteButton Button.Output
    | HandleEditMoneyItemInput EditMoneyItem.Output
    | NoAction CurrencyControl.Output -- try to do polymorphic

type Slots =
    ( editMoneyItem :: forall q. H.Slot q EditMoneyItem.Output Int
    , currencyControl :: forall q. H.Slot q CurrencyControl.Output Int
    , button :: forall q. H.Slot q Button.Output Int
    )

_button = Proxy :: Proxy "button"

_editMoneyItem = Proxy :: Proxy "editMoneyItem"

moneyItem :: forall m. MonadStore Store.Action Store.Store m => H.Component Query Input Output m
moneyItem =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery,
            initialize = Just Initialize, receive = Just <<< Receive }
        }
    where
    initialState i = { item: i, initialItemState: i, mode: View }
    handleAction :: forall slots. Action -> H.HalogenM State Action slots Output m Unit
    handleAction = case _ of
        Receive item -> do
            H.modify_ _ { item = item }
        HandleEditButton _ -> do
            H.modify_ _ { mode = Edit }
        HandleConfirmButton _ -> do
            { item: currentItem } <- H.get
            H.raise $ ConfirmedUpdate currentItem
            H.modify_ _ { mode = View }
        HandleDeleteButton _ -> do
            H.raise ClickedDelete
        HandleEditMoneyItemInput (UpdatedState item) -> do
            H.modify_ _ { item = item }
        _ -> do
            pure unit
    handleQuery :: forall slots a. (Query a) ->  H.HalogenM State Action slots Output m (Maybe a)
    handleQuery (ResetState a) = do
        { initialItemState } <- H.get
        H.modify_ _ { item = initialItemState }
        pure Nothing
    render :: State -> H.ComponentHTML Action Slots m
    render { item, mode } =
        HH.div
         []
         [ whenElem (mode == View) viewMoneyItem
         , whenElem (mode == Edit) editMoneyItem
         , button
         , deleteButton
         ]
        where
            button = case mode of
                View -> HH.slot _button 0 Button.button { text: "Edit" } HandleEditButton
                _ -> HH.slot _button 1 Button.button { text: "Confirm" } HandleConfirmButton
            deleteButton = HH.slot _button 2 Button.button { text: "Delete" } HandleDeleteButton
            viewMoneyItem _ = HH.div
                []
                [ HH.text item.name
                , HH.text $ toStringAs decimal item.amount
                , HH.slot _currencyControl 0 CurrencyControl.currencyControl { mode: View, currencyId: item.currencyId  } NoAction ]
            editMoneyItem _ = HH.slot _editMoneyItem 0 EditMoneyItem.editMoneyItem
             { name: item.name, currencyId: item.currencyId, amount: item.amount } HandleEditMoneyItemInput