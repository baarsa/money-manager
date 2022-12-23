module HTML.Utils where

import Prelude
import Data.Maybe (Maybe)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH

maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text "no"