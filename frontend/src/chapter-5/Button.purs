module Button where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (replicate)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Effect.Console (log)
import Effect.Aff.Class (class MonadAff)

type Input = { label :: String }

type State =
  { label :: String
  , amount :: Int
  , toggled :: Boolean
  }

data Output = Toggled Boolean

data Action = Receive Input | Click

data Query a
  = AddButton a
  | GetAmount (Int -> a)
  | SetAmount Int a

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ _ { label = input.label }

  Click -> do
    H.liftEffect $ log "hello"
    H.modify_ \state -> state { toggled = not state.toggled }
    toggled <- H.gets _.toggled
    H.raise $ Toggled toggled

initialState :: Input -> State
initialState input = { label: input.label, amount: 1, toggled: false }

render :: forall m. State -> H.ComponentHTML Action () m
render { label, amount, toggled } =
  HH.div_ $ replicate amount element
  where
    toggleText = (if toggled then "Rick" else "Morty")
    element = (
      HH.button
        [ HE.onClick \_ -> Click ]
        [ HH.text $ label <> toggleText ]
      )

handleQuery :: forall a m. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  AddButton a -> do
    H.modify_ \state -> state { amount = state.amount + 1 }
    H.liftEffect $ log "child AddButton"
    pure (Just a)

  GetAmount reply -> do
    amount <- H.gets _.amount
    -- ... do something, then provide the requested `Boolean` to the `reply`
    -- function to produce the `a` we need to return
    pure (Just (reply amount))

  SetAmount amount a -> do
    H.modify_ _ { amount = amount }
    pure (Just a)

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
    }
