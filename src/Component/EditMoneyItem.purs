module Component.EditMoneyItem where

import Prelude
import Data.Currency
import Component.StringInput as StringInput
import Component.CurrencyControl as CurrencyControl
import Component.NumberInput as NumberInput
import Type.Proxy
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import ComponentMode
import Data.Maybe
import Halogen.Store.Monad (class MonadStore)
import Store as Store

type Input = { name :: String
                     , currencyId :: Int
                     , amount :: Int }

data Output = UpdatedState Input

type State = Input

data Action = SetLocation String
    | SetCurrency Currency
    | SetAmount Int
    | Initialize
    | Receive Input
    | HandleNameInput StringInput.Output
    | HandleCurrencyOutput CurrencyControl.Output
    | HandleNumberInput NumberInput.Output

type Slots =
    ( stringInput :: forall q. H.Slot q StringInput.Output Int
    , currencyControl :: forall q. H.Slot q CurrencyControl.Output Int
    , numberInput :: forall q. H.Slot q NumberInput.Output Int
    )

_stringInput = Proxy :: Proxy "stringInput"

_currencyControl = Proxy :: Proxy "currencyControl"

_numberInput = Proxy :: Proxy "numberInput"

editMoneyItem :: forall q m. MonadStore Store.Action Store.Store m =>  H.Component q Input Output m
editMoneyItem = H.mkComponent
        { initialState: identity
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize, receive = Just <<< Receive }
        }
        where
        handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
        handleAction = case _ of
            Receive item -> H.put item
            HandleNameInput (StringInput.ValueUpdated newNameValue) -> do
                H.modify_ (\s -> s { name = newNameValue })
                s <- H.get
                H.raise $ UpdatedState s
            HandleCurrencyOutput (CurrencyControl.ChangedCurrency newCurId) -> do
                H.modify_ (\s -> s { currencyId = newCurId })
                s <- H.get
                H.raise $ UpdatedState s
            HandleNumberInput (NumberInput.ChangedValue newAmount) -> do
                H.modify_ (\s -> s { amount = newAmount })
                s <- H.get
                H.raise $ UpdatedState s
            _ -> pure unit
        render :: State -> H.ComponentHTML Action Slots m
        render { name, currencyId, amount } = HH.div
           []
           [ HH.slot _stringInput 0 StringInput.stringInput name HandleNameInput
           , HH.slot _numberInput 0 NumberInput.numberInput { value: amount } HandleNumberInput
           , HH.slot _currencyControl 0 CurrencyControl.currencyControl { mode: Edit, currencyId } HandleCurrencyOutput]