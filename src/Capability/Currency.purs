module Capability.Currency where

import Prelude
import Data.Currency (Currency)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageCurrencies m where
    getCurrencies :: m (Maybe (Array Currency))

instance ManageCurrencies m => ManageCurrencies (HalogenM st act slots msg m) where
    getCurrencies = lift getCurrencies