module Component.Notifications where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Data.Maybe
import Data.Array
import Effect.Random
import Effect.Aff.Class (class MonadAff)
import Effect.Aff as Aff
import Data.Time.Duration
import Data.Int
import HTML.Utils

showTime = 5000.0 -- time to disappear
maxId = 1000000

data NotificationLevel = Error | Warning | Success

type NotificationData = {
    message :: String,
    level :: NotificationLevel
}
--todo reuse ND
type NotificationDataWithId = {
    message :: String,
    level :: NotificationLevel,
    id :: Int
}

data Query a = PushNotification NotificationData a

type State = {
    notifications :: Array NotificationDataWithId
}

data Action = AddNotification NotificationData | RemoveNotification Int

notifications :: forall i o m. MonadAff m => H.Component Query i o m
notifications =
    H.mkComponent
        { initialState: \_ -> { notifications: [] }
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }}
    where
    handleAction :: forall slots output. Action -> H.HalogenM State Action slots output m Unit
    handleAction = case _ of
        RemoveNotification id -> do
            { notifications } <- H.get
            let mbArr = do
                   index <- findIndex (\item -> item.id == id) notifications
                   deleteAt index notifications
            case mbArr of
                Just arr -> H.modify_ _ { notifications = arr }
                _ -> pure unit
        _ -> pure unit
    handleQuery :: forall slots output a. (Query a) -> H.HalogenM State Action slots output m (Maybe a)
    handleQuery (PushNotification item a) = do
        id <- H.liftEffect $ randomInt 0 maxId
        let newItem = { id, message: item.message, level: item.level }
        { notifications } <- H.get
        H.modify_ _ { notifications = snoc notifications newItem }
        _ <- H.subscribe =<< (timer $ RemoveNotification id)
        -- schedule removal
        pure Nothing
    render :: State -> H.ComponentHTML Action () m
    render { notifications } =
        HH.div [ cssClass "notifications" ] $ map renderNotification notifications
        where
        renderNotification { id, level, message } =
            HH.div [ cssClass "notification" ] [ HH.text message ]
    timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
    timer val = do
        { emitter, listener } <- H.liftEffect HS.create
        _ <- H.liftAff $ Aff.forkAff $ do -- do we need fork?
            Aff.delay $ Milliseconds showTime
            H.liftEffect $ HS.notify listener val
        pure emitter
