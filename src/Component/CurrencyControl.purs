module Component.CurrencyControl where

import Prelude
import Data.Currency (Currency)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Store as Store
import Halogen.Store.Connect (connect)
import Data.Maybe
import Network.RemoteData
import Halogen.Store.Monad (class MonadStore)
import Data.Array (find)
import Halogen.Store.Connect (Connected)
import ComponentMode
import Data.String.CodeUnits
import Data.Int
import Data.Array

-- a controlled component. receives mode and currency id from MoneyItem.
-- output is new currency id (int)
-- connected to the store, selects currencies and a) use them as options for select b) gets current one by id
-- in view renders symbol + tooltip with name;
-- in edit renders select with options from store and current selected by id

type State = {
    mode :: ComponentMode,
    item :: Maybe Currency,
    availableCurrencies :: Array Currency
}

type Input = {
    mode :: ComponentMode,
    currencyId :: Int
}

data Action = Receive (Connected (RemoteData String (Array Currency)) Input) | ChangeCurrency (Maybe Int)

data Output = ChangedCurrency Int

currencyControl :: forall q m. MonadStore Store.Action Store.Store m => H.Component q Input Output m
currencyControl =
    connect Store.selectCurrencies $ H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
        }
        where
        initialState { context: currencies, input } =
            { mode: input.mode
            , item: getCurrencyById currencies input.currencyId
            , availableCurrencies: getCurrenciesArr currencies
            }
        handleAction :: forall slots. Action -> H.HalogenM State Action slots Output m Unit
        handleAction = case _ of
            Receive { context: currencies, input } -> do
                H.put
                    { mode: input.mode
                    , item: getCurrencyById currencies input.currencyId
                    , availableCurrencies: getCurrenciesArr currencies
                    }
            ChangeCurrency curId -> do
                case curId of
                    Just id -> H.raise $ ChangedCurrency id
                    Nothing -> pure unit
            _ -> pure unit
        render :: forall slots. State -> H.ComponentHTML Action slots m
        -- TODO implement invalid states and no selection
        render { mode, item, availableCurrencies } =
            case mode of
                View -> viewCurrency item
                Edit -> selectCurrency item
            where
            viewCurrency mbCur = case mbCur of
                Just cur -> HH.div [ HP.title cur.name ] [ HH.text $ fromCharArray [cur.symbol] ]
                _ -> HH.text ""
            selectCurrency mbCur = HH.select
                [ HE.onSelectedIndexChange (\ind -> ChangeCurrency (map _.id (index availableCurrencies ind) ) ) ] --rewrite
                (map (\cur2 -> HH.option
                    [ HP.value $ toStringAs decimal cur2.id
                    , HP.selected $ sameCurrency mbCur cur2 ] [ HH.text cur2.name ]) availableCurrencies)
        sameCurrency mbCur cur = case mbCur of
            Just cur2 -> cur2.id == cur.id
            _ -> false
        getCurrencyById :: RemoteData String (Array Currency) -> Int -> Maybe Currency
        getCurrencyById rd id =
            case rd of
                Success arr -> find (\x -> x.id == id) arr
                _ -> Nothing
        getCurrenciesArr :: RemoteData String (Array Currency) -> Array Currency
        getCurrenciesArr rd =
            case rd of
                Success arr -> arr
                _ -> []
