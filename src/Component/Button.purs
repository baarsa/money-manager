module Component.Button where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input = {
    text :: String
}

data Output = Clicked

data Action = Click

button :: forall query m. H.Component query Input Output m
button =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render { text } =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text text ]

  handleAction :: forall state. Action -> H.HalogenM state Action () Output m Unit
  handleAction = case _ of
    Click ->
      H.raise Clicked