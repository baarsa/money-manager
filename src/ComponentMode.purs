module ComponentMode where

import Prelude

data ComponentMode = View | Edit

derive instance Eq ComponentMode