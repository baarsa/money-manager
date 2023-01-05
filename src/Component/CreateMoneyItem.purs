module Component.CreateMoneyItem where

import Prelude
import Component.Button as Button
import Component.EditMoneyItem as EditMoneyItem
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy
import Store as Store
import Halogen.Store.Monad (class MonadStore)
import Component.EditMoneyItem

type State = { name :: String, currencyId :: Int, amount :: Int }

data Output = Confirmed State | Cancelled

data Action = HandleConfirmButton Button.Output
                  | HandleCancelButton Button.Output
                  | HandleEditMoneyItemInput EditMoneyItem.Output

type Slots =
    ( editMoneyItem :: forall q. H.Slot q EditMoneyItem.Output Int
    , button :: forall q. H.Slot q Button.Output Int
    )

_button = Proxy :: Proxy "button"

_editMoneyItem = Proxy :: Proxy "editMoneyItem"

createMoneyItem :: forall q i m. MonadStore Store.Action Store.Store m => H.Component q i Output m
createMoneyItem =
    H.mkComponent
        { initialState: \_ -> { name: "", currencyId: 0, amount: 0 } -- think about representing invalid states
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where
    handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
    handleAction = case _ of
        HandleEditMoneyItemInput (UpdatedState state) -> do
            H.put state
        HandleConfirmButton _ -> do
            state <- H.get
            H.raise $ Confirmed state
        HandleCancelButton _ -> do
            H.raise $ Cancelled
        _ -> pure unit
    render :: State -> H.ComponentHTML Action Slots m
    render { name, currencyId, amount } =
        HH.div []
            [ HH.slot _editMoneyItem 0 EditMoneyItem.editMoneyItem { name, amount, currencyId} HandleEditMoneyItemInput
            , HH.slot _button 0 Button.button { text: "Confirm" } HandleConfirmButton
            , HH.slot _button 1 Button.button { text: "Cancel" } HandleCancelButton
            ]
