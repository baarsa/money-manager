module Data.Currency where

import Prelude
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut (JsonCodec)

type Currency =
    { id :: Int
    , name :: String
    , symbol :: Char
    }

currencyCodec :: JsonCodec Currency
currencyCodec =
    CAR.object "Currency"
        { id: CA.int
        , name: CA.string
        , symbol: CA.char
        }

currenciesCodec :: JsonCodec (Array Currency)
currenciesCodec =
    CA.array currencyCodec