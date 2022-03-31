module Main where

import Prelude

import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Data.Functor.Indexed (class IxFunctor)
import Effect (Effect)
import Effect.Console (log)
import Prim.Row as Row
import Type.Row (type (+))

--
type IrrefutablyHasOutput r =
  ( irrefutablyHasOutput :: Unit
  | r
  )

type IrrefutablyMakesSound r =
  ( irrefutablyMakesSound :: Unit
  | r
  )

type ValidCapabilities r = IrrefutablyHasOutput + IrrefutablyMakesSound + r

type CannotAcceptInput r =
  ( cannotAcceptInput :: Unit
  | r
  )

--
data GraphBuilder :: Row Type -> Row Type -> Type -> Type
data GraphBuilder i o a = UnsafeGraphBuilder

type GraphBuilder' i = GraphBuilder i i

runGraphBuilder :: forall o a. GraphBuilder () o a -> Unit
runGraphBuilder _ = unit

instance Functor (GraphBuilder i i) where
  map _ _ = UnsafeGraphBuilder

instance Apply (GraphBuilder i i) where
  apply _ _ = UnsafeGraphBuilder

instance Applicative (GraphBuilder i i) where
  pure _ = UnsafeGraphBuilder

instance Bind (GraphBuilder i i) where
  bind _ _ = UnsafeGraphBuilder

instance Monad (GraphBuilder i i)

instance IxFunctor GraphBuilder where
  imap _ _ = UnsafeGraphBuilder

instance IxApply GraphBuilder where
  iapply _ _ = UnsafeGraphBuilder

instance IxApplicative GraphBuilder where
  ipure _ = UnsafeGraphBuilder

instance IxBind GraphBuilder where
  ibind _ _ = UnsafeGraphBuilder

instance IxMonad GraphBuilder

--
data GraphElement :: Row Type -> Row Type -> Type
data GraphElement capabilities restrictions = UnsafeGraphElement

--
createSpeaker :: forall capabilities. GraphBuilder' capabilities (GraphElement (IrrefutablyHasOutput + ()) ())
createSpeaker = UnsafeGraphBuilder

createGain :: forall capabilities. GraphBuilder' capabilities (GraphElement () ())
createGain = UnsafeGraphBuilder

createSource :: forall capabilities. GraphBuilder' capabilities (GraphElement (IrrefutablyMakesSound + ()) (CannotAcceptInput + ()))
createSource = UnsafeGraphBuilder

connect
  :: forall fromC fromR intoC intoR fromIntoC prevC nextC
   . Row.Lacks "cannotAcceptInput" intoR
  => Row.Union fromC intoC fromIntoC
  => Row.Union fromIntoC prevC nextC
  => { from :: GraphElement fromC fromR
     , into :: GraphElement intoC intoR
     }
  -> GraphBuilder prevC nextC Unit
connect _ = UnsafeGraphBuilder

--
yes :: Unit
yes = runGraphBuilder Ix.do
  speaker <- createSpeaker
  gain <- createGain
  source <- createSource
  connect { from: source, into: gain }
  connect { from: gain, into: speaker }

-- no = runGraphBuilder $ Ix.do
--   source <- createSource
--   connect { from: source, into: source }

main :: Effect Unit
main = log "🍝"
