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
import Effect.Console (log)
import Effect.Random (random)
import Effect.Random (randomInt)
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

-- state: item data without id; mode: view | edit;
-- output: on save (moneyItem), on delete ()

type Input = { name :: String
                     , currencyId :: Int
                     , amount :: Int }

data Output = ConfirmedUpdate Input

type State = {
    item :: Input,
    mode :: ComponentMode
}

data Action = SetLocation String
    | SetCurrency Currency
    | SetAmount Int
    | Initialize
    | Receive Input
    | HandleEditButton Button.Output
    | HandleConfirmButton Button.Output
    | HandleNameInput StringInput.Output
    | HandleCurrencyOutput CurrencyControl.Output
    | HandleNumberInput NumberInput.Output

type Slots =
    ( stringInput :: forall q. H.Slot q StringInput.Output Int
    , button :: forall q. H.Slot q Button.Output Int
    , currencyControl :: forall q. H.Slot q CurrencyControl.Output Int
    , numberInput :: forall q. H.Slot q NumberInput.Output Int
    )

_button = Proxy :: Proxy "button"

_stringInput = Proxy :: Proxy "stringInput"

_currencyControl = Proxy :: Proxy "currencyControl"

_numberInput = Proxy :: Proxy "numberInput"

moneyItem :: forall q m. MonadStore Store.Action Store.Store m => H.Component q Input Output m
moneyItem =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize, receive = Just <<< Receive }
        }
    where
    initialState i = { item: i, mode: View }
    handleAction :: forall slots. Action -> H.HalogenM State Action slots Output m Unit
    handleAction = case _ of
        Receive item -> do
            H.modify_ _ { item = item }
        HandleEditButton _ -> do
            H.modify_ _ { mode = Edit }
        HandleConfirmButton _ -> do
            -- send output with current state
            { item: currentItem } <- H.get
            H.raise $ ConfirmedUpdate currentItem
            -- if success do nothing else show fail notification (later)
            H.modify_ _ { mode = View }
        HandleNameInput (StringInput.ValueUpdated newNameValue) -> do
            H.modify_ (\s -> s { item = s.item { name = newNameValue } })
        HandleCurrencyOutput (CurrencyControl.ChangedCurrency newCurId) -> do
            H.modify_ (\s -> s { item = s.item { currencyId = newCurId } })
        HandleNumberInput (NumberInput.ChangedValue newAmount) -> do
            H.modify_ (\s -> s { item = s.item { amount = newAmount } })
        _ -> do
            pure unit
    render :: State -> H.ComponentHTML Action Slots m
    render { item, mode } =
        HH.div
         []
         [ name
         , amount
         , HH.slot _currencyControl 0 CurrencyControl.currencyControl { mode, currencyId: item.currencyId } HandleCurrencyOutput
         , button
         ]
        where
            button = case mode of
                View -> HH.slot _button 0 Button.button { text: "Edit" } HandleEditButton
                _ -> HH.slot _button 1 Button.button { text: "Confirm" } HandleConfirmButton
            name = case mode of
                View -> HH.text item.name
                _ -> HH.slot _stringInput 0 StringInput.stringInput item.name HandleNameInput
            amount = case mode of
                 View -> HH.text $ toStringAs decimal item.amount
                 _ -> HH.slot _numberInput 0 NumberInput.numberInput { value: item.amount } HandleNumberInput