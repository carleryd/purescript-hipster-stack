module Chapter4Sub where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Initialize | Tick | Unsubscribe | SetTimer Int

type State = {
  seconds :: Int,
  subscriptionId :: Maybe H.SubscriptionId
}

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: forall input. input -> State
initialState _ = {
  seconds: 0,
  subscriptionId: Nothing
}

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_
               [ HH.text ("You have been here for " <> show state.seconds <> " seconds")
               , HH.button
                 [ HE.onClick \_ -> Unsubscribe ]
                 [ HH.text "Unsubscribe counter" ]
               , HH.button
                 [ HE.onClick \_ -> Initialize ]
                 [ HH.text "Restart" ]
               , HH.button
                 [ HE.onClick \_ -> SetTimer 0 ]
                 [ HH.text "Clear" ]
               ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    subscriptionId <- H.subscribe =<< (timer Tick)
    H.modify_ _ { subscriptionId = Just subscriptionId }
    pure unit

  Tick ->
    H.modify_ \state -> state { seconds = state.seconds + 1 }

  Unsubscribe -> do
    subscriptionId <- H.gets _.subscriptionId
    case subscriptionId of
      Just id -> H.unsubscribe id
      Nothing -> pure unit

  SetTimer newSeconds ->
    H.modify_ _ { seconds = newSeconds }

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter
