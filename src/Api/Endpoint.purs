module Api.Endpoint where

import Prelude
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', int, prefix, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Endpoint
    = MoneyItem Int
    | MoneyItems
    | Currencies

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
    { "MoneyItem": "money-item" / int segment
    , "MoneyItems": "money-items" / noArgs
    , "Currencies": "currencies" / noArgs
    }
