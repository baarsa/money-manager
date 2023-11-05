module HTML.Utils where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""

elemsByCondition :: forall p i. Boolean -> HH.HTML p i -> HH.HTML p i -> HH.HTML p i
elemsByCondition true first _ = first
elemsByCondition _ _ second = second

cssClass :: forall r i. String -> HH.IProp (class :: String | r) i
cssClass = HP.class_ <<< HH.ClassName