module Capability.MoneyItem where

import Prelude
import Data.MoneyItem (MoneyItem, MoneyItemWithId)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageMoneyItems m where
    getMoneyItems :: Unit -> m (Maybe (Array MoneyItemWithId))
    createMoneyItem :: MoneyItem -> m (Maybe MoneyItemWithId)
    updateMoneyItem :: MoneyItemWithId -> m (Maybe MoneyItemWithId)
    deleteMoneyItem :: Int -> m Unit -- why was there m Maybe, but here just m

instance ManageMoneyItems m => ManageMoneyItems (HalogenM st act slots msg m) where
    getMoneyItems = lift <<< getMoneyItems
    createMoneyItem = lift <<< createMoneyItem
    updateMoneyItem = lift <<< updateMoneyItem
    deleteMoneyItem = lift <<< deleteMoneyItem
