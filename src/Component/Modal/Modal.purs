module Component.Modal where

import Prelude
import Type.Proxy
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as H
import Component.Button as Button
import HTML.Utils (cssClass)

type Input = { title :: String, text :: String }

type State = Input

data Action = HandleCloseClick Button.Output | HandleConfirmClick Button.Output

data Output = Closed | Confirmed

type Slots =
    (  button :: forall q. H.Slot q Button.Output Int
    )

_button = Proxy :: Proxy "button"

modal :: forall q m. H.Component q Input Output m
modal =
    H.mkComponent
        { initialState: identity
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction } }
    where
    handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
    handleAction (HandleCloseClick _) = H.raise Closed
    handleAction (HandleConfirmClick _) = H.raise Confirmed
    render :: State -> H.ComponentHTML Action Slots m
    render { title, text } =
        HH.div_
            [ HH.div [ cssClass "modal-backdrop" ] []
            , HH.div [ cssClass "modal" ]
                [ HH.h2 [] [HH.text title]
                , HH.div [] [HH.text text]
                , HH.div [ cssClass "modal-buttons" ]
                    [ HH.slot _button 0 Button.button { text: "OK" } HandleConfirmClick
                    , HH.slot _button 0 Button.button { text: "Cancel" } HandleCloseClick ]
                ]
            ]