module Chapter5 where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Button as Button

type Slots = ( button :: H.Slot Button.Query Button.Output Int )

data Action = Initialize | Tick | HandleButton Button.Output | IncrementButtons

type State = Int

_button = Proxy :: Proxy "button"

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ HH.slot _button 0 Button.component { label: ("Count: " <> show state)} HandleButton
    , HH.button [ HE.onClick \_ -> IncrementButtons ] [ HH.text "ADD MORE!!" ]
    ]

initialState :: forall input. input -> State
initialState _ = 0

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.subscribe =<< (timer Tick)
    pure unit

  Tick ->
    H.modify_ \state -> state + 1

  IncrementButtons -> do
    H.tell _button 0 Button.AddButton

  HandleButton _ ->
    pure unit
  -- HandleButton output ->
  --   case output of
  --     Button.Clicked -> do
  --       amount <- H.request _button 0 Button.GetAmount
  --       H.liftEffect $ log $ "click from parent updated" <> show amount

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
