module Main where

import Prelude

import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Data.Functor.Indexed (class IxFunctor)
import Effect (Effect)
import Effect.Console (log)
import Prim.Boolean (False, True)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--
data Tuple :: forall k l. k -> l -> Type
data Tuple a b

infixr 6 type Tuple as /\

--
data GraphBuilder :: Type -> Type -> Type -> Type
data GraphBuilder i o a = UnsafeGraphBuilder

type GraphBuilder' i = GraphBuilder i i

evalGraphBuilder :: forall s r. GraphBuilder InitialGraphBuilderIndex (_ /\ s /\ r) _ -> Proxy (s /\ r)
evalGraphBuilder _ = Proxy

type InitialGraphBuilderIndex :: Type
type InitialGraphBuilderIndex = "_" /\ (() :: Row Type) /\ (() :: Row Type)

instance functorGraphBuilder :: Functor (GraphBuilder i i) where
  map _ _ = UnsafeGraphBuilder

instance applyGraphBuilder :: Apply (GraphBuilder i i) where
  apply _ _ = UnsafeGraphBuilder

instance applicativeGraphBuilder :: Applicative (GraphBuilder i i) where
  pure _ = UnsafeGraphBuilder

instance bindGraphBuilder :: Bind (GraphBuilder i i) where
  bind _ _ = UnsafeGraphBuilder

instance monadGraphBuilder :: Monad (GraphBuilder i i)

instance ixFunctorGraphBuilder :: IxFunctor GraphBuilder where
  imap _ _ = UnsafeGraphBuilder

instance ixApplyGraphBuilder :: IxApply GraphBuilder where
  iapply _ _ = UnsafeGraphBuilder

instance ixApplicativeGraphBuilder :: IxApplicative GraphBuilder where
  ipure _ = UnsafeGraphBuilder

instance ixBindGraphBuilder :: IxBind GraphBuilder where
  ibind _ _ = UnsafeGraphBuilder

instance ixMonadGraphBuilder :: IxMonad GraphBuilder

--
data GraphUnit :: Row Type -> Row Type -> Type
data GraphUnit capabilities restrictions = UnsafeGraphUnit

--
class Allocate :: Type -> Type -> Constraint
class Allocate i o | i -> o

instance allocate ::
  ( Symbol.Cons "_" currentId futureId
  , Row.Cons currentId Unit currentSet futureSet
  ) =>
  Allocate (currentId /\ currentSet /\ currentRef) (futureId /\ futureSet /\ currentRef)

class Deallocate :: Symbol -> Type -> Type -> Constraint
class Deallocate l i o | i -> o

instance deallocate ::
  ( Row.Cons idToDeallocate Unit futureSet currentSet
  ) =>
  Deallocate idToDeallocate
    (currentId /\ currentSet /\ currentRef)
    (currentId /\ futureSet /\ currentRef)

--
createSpeaker
  :: forall i0 i1 i2 o
   . Allocate (i0 /\ i1 /\ i2) o
  => GraphBuilder (i0 /\ i1 /\ i2) o (Proxy (i0 /\ True /\ False))
createSpeaker = UnsafeGraphBuilder

createGain
  :: forall i0 i1 i2 o
   . Allocate (i0 /\ i1 /\ i2) o
  => GraphBuilder (i0 /\ i1 /\ i2) o (Proxy (i0 /\ False /\ False))
createGain = UnsafeGraphBuilder

createSinOsc
  :: forall i0 i1 i2 o
   . Allocate (i0 /\ i1 /\ i2) o
  => GraphBuilder (i0 /\ i1 /\ i2) o (Proxy (i0 /\ False /\ False))
createSinOsc = UnsafeGraphBuilder

intoRef
  :: forall i0 i1 i2 j1 j2 l t
   . Row.Cons l Unit j1 i1
  => Row.Cons l Unit i2 j2
  => Proxy (l /\ t /\ False)
  -> GraphBuilder (i0 /\ i1 /\ i2) (i0 /\ j1 /\ j2) (Proxy (l /\ t /\ True))
intoRef _ = UnsafeGraphBuilder

dropRef
  :: forall i0 i1 i2 o l t
   . Row.Lacks l i2
  => Proxy (l /\ t /\ True)
  -> GraphBuilder (i0 /\ i1 /\ i2) (i0 /\ i1 /\ i2) Unit
dropRef _ = UnsafeGraphBuilder

class Connect f t i o a | f t i -> o a where
  connect :: Proxy f -> Proxy t -> GraphBuilder i o (Proxy a)

instance connectIntoSelf ::
  ( Row.Cons i Unit currentSet' currentSet
  , Row.Cons currentId Unit currentSet' futureSet
  , Symbol.Cons "_" currentId futureId
  ) =>
  Connect (i /\ t /\ False)
    (i /\ t /\ False)
    (currentId /\ currentSet /\ currentRef)
    (futureId /\ futureSet /\ currentRef)
    (currentId /\ t /\ False) where
  connect _ _ = UnsafeGraphBuilder

else instance connectNoTerminals ::
  ( Row.Cons i0 Unit currentSet' currentSet
  , Row.Cons i1 Unit currentSet'' currentSet'
  , Row.Cons currentId Unit currentSet'' futureSet
  , Symbol.Cons "_" currentId futureId
  ) =>
  Connect (i0 /\ False /\ False)
    (i1 /\ False /\ False)
    (currentId /\ currentSet /\ currentRef)
    (futureId /\ futureSet /\ currentRef)
    (currentId /\ False /\ False) where
  connect _ _ = UnsafeGraphBuilder

else instance connectIntoTerminal ::
  ( Row.Cons i0 Unit currentSet' currentSet
  , Row.Cons i1 Unit currentSet'' currentSet'
  , Symbol.Cons "_" currentId futureId
  ) =>
  Connect (i0 /\ False /\ False)
    (i1 /\ True /\ False)
    (currentId /\ currentSet /\ currentRef)
    (futureId /\ currentSet'' /\ currentRef)
    (currentId /\ True /\ False) where
  connect _ _ = UnsafeGraphBuilder

basic = evalGraphBuilder Ix.do
  speaker <- createSpeaker
  gain <- createGain
  sinOsc <- createSinOsc
  _0 <- connect sinOsc gain
  _1 <- connect _0 speaker
  Ix.pure unit

selfConnect = evalGraphBuilder Ix.do
  speaker <- createSpeaker
  gain <- createGain
  _0 <- connect gain gain
  _1 <- connect _0 speaker
  Ix.pure unit

main :: Effect Unit
main = log "🍝"
