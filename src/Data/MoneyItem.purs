module Data.MoneyItem where

import Prelude
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut (JsonCodec)
import Type.Row (type (+))

type MoneyItemRep row =
    ( name :: String
    , currencyId :: Int
    , amount :: Int
    | row
    )

type MoneyItemMetadataRep row =
    ( id :: Int
    | row
    )

type MoneyItem = { | MoneyItemRep () }

type MoneyItemWithId = { | MoneyItemRep + MoneyItemMetadataRep () }

moneyItemCodec :: JsonCodec MoneyItem
moneyItemCodec =
    CAR.object "MoneyItemWithId"
        { name: CA.string
        , currencyId: CA.int
        , amount: CA.int
        }

moneyItemWithIdCodec :: JsonCodec MoneyItemWithId
moneyItemWithIdCodec =
    CAR.object "MoneyItemWithId"
        { id: CA.int
        , name: CA.string
        , currencyId: CA.int
        , amount: CA.int
        }

moneyItemsWithIdCodec :: JsonCodec (Array MoneyItemWithId)
moneyItemsWithIdCodec =
    CA.array moneyItemWithIdCodec