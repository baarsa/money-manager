module Component.NumberInput where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe
import Data.Int
import DOM.HTML.Indexed.InputType

type Input = {
    value :: Int
}

type State = {
    value :: Int
}

data Action = Receive Input | ChangeValue (Maybe Int)

data Output = ChangedValue Int

numberInput :: forall q m. H.Component q Input Output m
numberInput = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
    where
    initialState = identity
    handleAction :: forall slots. Action -> H.HalogenM State Action slots Output m Unit
    handleAction = case _ of
        Receive newValue -> H.put newValue
        ChangeValue mbValue -> do
            case mbValue of
                Just value -> H.raise $ ChangedValue value
                _ -> pure unit
        _ -> pure unit
    render { value } =
        HH.input
            [ HP.type_ InputNumber
            , HP.value $ toStringAs decimal value
            , HE.onValueInput (\val -> ChangeValue $ fromString val) ]