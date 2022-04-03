module Main where

import Prelude

import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Data.Functor.Indexed (class IxFunctor)
import Prim.Boolean (False, True)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Proxy (Proxy(..))

--
data Tuple :: forall k l. k -> l -> Type
data Tuple a b

infixr 6 type Tuple as /\

--
data GraphBuilder :: Type -> Type -> Type -> Type
data GraphBuilder i o a = UnsafeGraphBuilder

type InitialGraphBuilderIndex :: Type
type InitialGraphBuilderIndex = "_" /\ (() :: Row Type) /\ (() :: Row Boolean) /\ False

evalGraphBuilder :: forall o a. GraphBuilder InitialGraphBuilderIndex o a -> Proxy o
evalGraphBuilder _ = Proxy

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
data Speaker = Speaker

data Gain = Gain

data SinOsc = SinOsc

data GraphUnit :: Symbol -> Type -> Type
data GraphUnit id node = GraphUnit

class IsNode :: Type -> Constraint
class IsNode node

instance isNodeSpeaker :: IsNode Speaker
instance isNodeGain :: IsNode Gain
instance isNodeSinOsc :: IsNode SinOsc

class HasInput :: Type -> Constraint
class HasInput node

instance hasInputSpeaker :: HasInput Speaker
instance hasInputGain :: HasInput Gain

class HasOutput :: Type -> Constraint
class HasOutput node

instance hasOutputSpeaker :: HasOutput Speaker
instance hasOutputGain :: HasOutput Gain
instance hasOutputSinOsc :: HasOutput SinOsc

class HasSound :: Type -> Boolean -> Constraint
class HasSound node yesOrNo | node -> yesOrNo

instance hasSoundSinOsc :: HasSound SinOsc True
else instance hasSoundDefault :: HasSound node False

--
class Create :: Type -> Symbol -> Type -> Type -> Constraint
class Create i id node o | i node -> id o where
  create :: node -> GraphBuilder i o (GraphUnit id node)

instance createSpeaker ::
  ( Row.Cons n True t t'
  , Symbol.Cons "_" n n'
  ) =>
  Create (n /\ c /\ t /\ s) n Speaker (n' /\ c /\ t' /\ s) where
  create _ = UnsafeGraphBuilder

else instance createNode ::
  ( Row.Cons n False t t'
  , Symbol.Cons "_" n n'
  ) =>
  Create (n /\ c /\ t /\ s) n node (n' /\ c /\ t' /\ s) where
  create _ = UnsafeGraphBuilder

class IntoIsTerminal :: Boolean -> Constraint
class IntoIsTerminal isTerminal

instance intoIsTerminalTrue :: IntoIsTerminal True

class MakesSound :: Type -> Boolean -> Boolean -> Constraint
class MakesSound node n f | node n -> f

instance makesSoundAlready :: MakesSound node True True
else instance makesSoundFuture :: (HasSound node yesOrNo) => MakesSound node tOrF yesOrNo

class Connect :: Type -> Symbol -> Type -> Symbol -> Type -> Type -> Constraint
class Connect i fId fNode iId iNode o | i fId fNode iId iNode -> o where
  connect
    :: { from :: GraphUnit fId fNode
       , into :: GraphUnit iId iNode
       }
    -> GraphBuilder i o Unit

instance connectDefault ::
  ( HasOutput fNode
  , HasInput iNode
  , Row.Cons iId iIsTerminal t_ t
  , IntoIsTerminal iIsTerminal
  , Row.Cons fId tOrF t' t
  , Row.Cons fId True t' t''
  , Symbol.Cons "." iId iId'
  , Symbol.Append fId iId' cId
  , Row.Lacks cId c
  , Row.Cons cId Unit c c'
  , MakesSound fNode s s'
  ) =>
  Connect (n /\ c /\ t /\ s) fId fNode iId iNode (n /\ c' /\ t'' /\ s') where
  connect _ = UnsafeGraphBuilder
