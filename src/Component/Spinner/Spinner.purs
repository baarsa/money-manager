module Component.Spinner where

import Prelude
import Halogen.Component (Component, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML.Elements (div, div_) as HH
import HTML.Utils (cssClass)
import Halogen.HTML (ComponentHTML) as H
import Data.Array (replicate)

spinner :: forall q m. H.Component q Unit Unit m
spinner =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval
        }
        where
        initialState = const unit
        render :: forall slots state action. state -> H.ComponentHTML action slots m
        render = const $ HH.div [ cssClass "spinner" ] (replicate 4 $ HH.div_ [])