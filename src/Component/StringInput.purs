module Component.StringInput where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.Common (ClassName(..))
import Data.Maybe (Maybe(..))

type Input = String

type State = String

data Action = Receive String | UpdateValue String

data Output = ValueUpdated String

stringInput :: forall m q. H.Component q Input Output m
stringInput =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { receive = Just <<< Receive, handleAction = handleAction }
        }
    where
        initialState = identity
        handleAction :: forall slots. Action -> H.HalogenM State Action slots Output m Unit
        handleAction = case _ of
            UpdateValue newValue -> do
                H.raise (ValueUpdated newValue)
            Receive newValue -> do
                H.put newValue
            _ -> pure unit
        render :: forall slots. State -> H.ComponentHTML Action slots m
        render text =
            HH.input
                [ HP.value text
                , HE.onValueInput UpdateValue
                ]


