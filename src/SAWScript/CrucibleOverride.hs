{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module SAWScript.CrucibleOverride
  ( OverrideMatcher(..)
  , runOverrideMatcher

  , setupValueSub
  , executeFreshPointer
  , osAsserts
  , termSub

  , learnCond
  , matchArg
  , methodSpecHandler
  , valueToSC
  , termId
  ) where

import           Control.Lens
import           Control.Exception as X
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Either (partitionEithers)
import           Data.Foldable (for_, traverse_)
import           Data.List (tails)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.IORef (readIORef)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V

import qualified Text.LLVM.AST as L

import qualified Cryptol.TypeCheck.AST as Cryptol (Schema(..))
import qualified Cryptol.Eval.Type as Cryptol (TValue(..), evalType)

import qualified Lang.Crucible.CFG.Core as Crucible
                   (TypeRepr(UnitRepr), GlobalVar,
                    BaseTypeRepr(..))
import qualified Lang.Crucible.Simulator.OverrideSim as Crucible
import qualified Lang.Crucible.Simulator.GlobalState as Crucible
import qualified Lang.Crucible.Simulator.RegMap as Crucible
import qualified Lang.Crucible.Simulator.SimError as Crucible

import qualified Lang.Crucible.LLVM as Crucible
import qualified Lang.Crucible.LLVM.Bytes as Crucible
import qualified Lang.Crucible.LLVM.Extension as Crucible
import qualified Lang.Crucible.LLVM.MemType as Crucible
import qualified Lang.Crucible.LLVM.LLVMContext as TyCtx
import qualified Lang.Crucible.LLVM.Translation as Crucible
import qualified Lang.Crucible.LLVM.MemModel as Crucible
import qualified Lang.Crucible.LLVM.MemModel.Type as Crucible
import qualified Lang.Crucible.LLVM.MemModel.Pointer as Crucible
import qualified Lang.Crucible.Solver.Interface as Crucible
import qualified Lang.Crucible.Solver.SAWCoreBackend as Crucible
import qualified Lang.Crucible.Solver.SimpleBuilder as Crucible
import qualified Lang.Crucible.Solver.Symbol as Crucible
import qualified Lang.Crucible.ProgramLoc as Crucible

import qualified Data.Parameterized.TraversableFC as Ctx
import qualified Data.Parameterized.Context as Ctx

import           Verifier.SAW.SharedTerm
import           Verifier.SAW.Prelude (scEq)
import           Verifier.SAW.TypedAST
import           Verifier.SAW.Recognizer

import           SAWScript.CrucibleMethodSpecIR
import           SAWScript.CrucibleResolveSetupValue
import           SAWScript.TypedTerm
import           SAWScript.Options
import           SAWScript.Exceptions (failRuntimeIO)
import           SAWScript.Utils (handleException)

-- | The 'OverrideMatcher' type provides the operations that are needed
-- to match a specification's arguments with the arguments provided by
-- the Crucible simulation in order to compute the variable substitution
-- and side-conditions needed to proceed.
newtype OverrideMatcher arch a =
  OM (StateT (OverrideState arch) (ExceptT OverrideFailure IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

data OverrideState arch = OverrideState
  { -- | Substitution for memory allocations
    _setupValueSub :: Map AllocIndex (LLVMPtr (Crucible.ArchWidth arch))

    -- | Substitution for SAW Core external constants
  , _termSub :: Map VarIndex Term

    -- | Free variables available for unification
  , _osFree :: Set VarIndex

    -- | Accumulated assertions
  , _osAsserts :: [(Crucible.Pred Sym, Crucible.SimErrorReason)]

    -- | Accumulated assumptions
  , _osAssumes :: [Crucible.Pred Sym]

    -- | Symbolic simulation state
  , _syminterface :: Sym

    -- | Global variables
  , _overrideGlobals :: Crucible.SymGlobalState Sym
  }

data OverrideFailure
  = BadSymType Crucible.SymType
  | AmbiguousPointsTos [PointsTo]
  | AmbiguousVars [TypedTerm]
  | BadTermMatch Term Term -- ^ simulated and specified terms did not match
  | BadPointerCast -- ^ Pointer required to process points-to
  | BadReturnSpecification -- ^ type mismatch in return specification
  | NonlinearPatternNotSupported
  | BadPointerLoad String -- ^ loadRaw failed due to type error
  | StructuralMismatch (Crucible.LLVMVal Sym)
                       SetupValue
                       Crucible.MemType
                        -- ^ simulated value, specified value, specified type
  deriving Show

instance Exception OverrideFailure

makeLenses ''OverrideState

------------------------------------------------------------------------

-- | The initial override matching state starts with an empty substitution
-- and no assertions or assumptions.
initialState ::
  Sym                           {- ^ simulator                      -} ->
  Crucible.SymGlobalState Sym   {- ^ initial global variables       -} ->
  Map AllocIndex (LLVMPtr (Crucible.ArchWidth arch)) {- ^ initial allocation substituion -} ->
  Map VarIndex Term             {- ^ initial term substituion       -} ->
  Set VarIndex                  {- ^ initial free terms             -} ->
  OverrideState arch
initialState sym globals allocs terms free = OverrideState
  { _osAsserts       = []
  , _osAssumes       = []
  , _syminterface    = sym
  , _overrideGlobals = globals
  , _termSub         = terms
  , _osFree          = free
  , _setupValueSub   = allocs
  }

------------------------------------------------------------------------

addAssert ::
  Crucible.Pred Sym       {- ^ property -} ->
  Crucible.SimErrorReason {- ^ reason   -} ->
  OverrideMatcher arch ()
addAssert p r = OM (osAsserts %= cons (p,r))

addAssume ::
  Crucible.Pred Sym       {- ^ property -} ->
  OverrideMatcher arch ()
addAssume p = OM (osAssumes %= cons p)

readGlobal ::
  Crucible.GlobalVar tp ->
  OverrideMatcher arch (Crucible.RegValue Sym tp)
readGlobal k =
  do mb <- OM (uses overrideGlobals (Crucible.lookupGlobal k))
     case mb of
       Nothing -> failRuntimeIO $ "No such global: " ++ show k
       Just v  -> return v

writeGlobal ::
  Crucible.GlobalVar    tp ->
  Crucible.RegValue Sym tp ->
  OverrideMatcher arch ()
writeGlobal k v = OM (overrideGlobals %= Crucible.insertGlobal k v)

------------------------------------------------------------------------

-- | Abort the current computation by raising the given 'OverrideFailure'
-- exception.
failure :: OverrideFailure -> OverrideMatcher arch a
failure e = OM (lift (throwE e))

------------------------------------------------------------------------

methodSpecHandler ::
  forall arch rtp args ret.
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  Options                  {- ^ output/verbosity options                     -} ->
  SharedContext            {- ^ context for constructing SAW terms           -} ->
  CrucibleContext arch     {- ^ context for interacting with Crucible        -} ->
  [CrucibleMethodSpecIR]   {- ^ specification for current function override  -} ->
  Crucible.TypeRepr ret    {- ^ type representation of function return value -} ->
  Crucible.OverrideSim Crucible.SAWCruciblePersonality Sym (Crucible.LLVM arch) rtp args ret
     (Crucible.RegValue Sym ret)
methodSpecHandler opts sc cc css retTy = do
  let L.Symbol fsym = (head css)^.csName
  globals <- Crucible.readGlobals
  sym     <- Crucible.getSymInterface
  (Crucible.RegMap args) <- Crucible.getOverrideArgs

  gs <- liftIO (buildGlobalsList sym (length css) globals)

  matches
     <- liftIO $
        zipWithM
          (\g cs ->
             let initialFree = Set.fromList (map (termId . ttTerm)
                                                 (view (csPreState.csFreshVars) cs))
             in runOverrideMatcher sym g Map.empty Map.empty initialFree
                  (methodSpecHandler1 opts sc cc args retTy cs))
          gs css

  outputs <- case partitionEithers matches of
               (e,[]  ) -> failRuntimeIO $ "All overrides failed: " ++ show e
               (_,s:ss) -> return (s:|ss)

  Crucible.writeGlobals =<< liftIO (muxGlobal sym (fmap snd outputs))

  liftIO $
    do -- assert the disjunction of all the preconditions
       do ps <- traverse (conjunction sym . toListOf (_2 . osAsserts . folded . _1)) outputs
          p  <- disjunction sym ps
          Crucible.sbAddAssertion (cc^.ccBackend) p
            (Crucible.AssertFailureSimError ("No applicable override for " ++ fsym))

       -- Postcondition can be used if precondition holds
       for_ outputs $ \(_,output) ->
         do p       <- conjunction sym (toListOf (osAsserts . folded . _1) output)
            q       <- conjunction sym (view osAssumes output)
            p_imp_q <- Crucible.impliesPred sym p q
            Crucible.sbAddAssumption (cc^.ccBackend) p_imp_q

       muxReturnValue sym retTy outputs

-- | When two global states are merged, only the writes since the last branch
-- are actually merged. Therefore we need to add enough branches to the global
-- states so that as we merge all of the results of applying overrides that there
-- are enough branches left over at the end to mux.
buildGlobalsList :: Sym -> Int -> Crucible.SymGlobalState Sym ->
  IO [Crucible.SymGlobalState Sym]
buildGlobalsList _   1 g = return [g]
buildGlobalsList sym n g =
  do g1 <- Crucible.globalPushBranch sym intrinsics g
     gs <- buildGlobalsList sym (n-1) g1
     return (g1:gs)

-- | Compute the conjunction of a set of predicates.
conjunction :: Foldable t => Sym -> t (Crucible.Pred Sym) -> IO (Crucible.Pred Sym)
conjunction sym = foldM (Crucible.andPred sym) (Crucible.truePred sym)

-- | Compute the disjunction of a set of predicates.
disjunction :: Foldable t => Sym -> t (Crucible.Pred Sym) -> IO (Crucible.Pred Sym)
disjunction sym = foldM (Crucible.orPred sym) (Crucible.falsePred sym)

-- | Compute the return value from a list of structurally matched
-- overrides. The result will be a muxed value guarded by the
-- preconditions of each of the overrides.
muxReturnValue ::
  Sym                   {- ^ symbolic simulator parameters -} ->
  Crucible.TypeRepr ret {- ^ type of return value          -} ->
  NonEmpty (Crucible.RegValue Sym ret, OverrideState arch)
                        {- ^ possible overrides            -} ->
  IO (Crucible.RegValue Sym ret) {- ^ muxed return value   -}
muxReturnValue _   _     ((val,_):|[]) = return val
muxReturnValue sym retTy ((val,x):|y:z) =
  do ys   <- muxReturnValue sym retTy (y:|z)
     here <- conjunction sym (map fst (view osAsserts x))
     Crucible.muxRegForType sym intrinsics retTy here val ys

muxGlobal :: Sym -> NonEmpty (OverrideState arch) -> IO (Crucible.SymGlobalState Sym)
muxGlobal _ (x:|[]) = return (view overrideGlobals x)
muxGlobal sym (x:|y:z) =
  do ys   <- muxGlobal sym (y:|z)
     here <- conjunction sym (toListOf (osAsserts . folded . _1) x)
     globalMuxUnleveled sym here (view overrideGlobals x) ys

-- | This mux function can handle cases wher the right-hand side has
-- more branches that the left-hand side. This can happen when an
-- override specification was aborted due to structural mismatch.
globalMuxUnleveled ::
  Sym -> Crucible.MuxFn (Crucible.Pred Sym) (Crucible.SymGlobalState Sym)
globalMuxUnleveled sym p l r
  | Crucible._globalPendingBranches l < Crucible._globalPendingBranches r =
     do r' <- Crucible.globalAbortBranch sym intrinsics r
        globalMuxUnleveled sym p l r'
  | otherwise = Crucible.globalMuxFn sym intrinsics p l r

------------------------------------------------------------------------

methodSpecHandler1 ::
  forall arch ret ctx.
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  Options                  {- ^ output/verbosity options                     -} ->
  SharedContext            {- ^ context for constructing SAW terms           -} ->
  CrucibleContext arch     {- ^ context for interacting with Crucible        -} ->
  Ctx.Assignment (Crucible.RegEntry Sym) ctx
           {- ^ type representation of function return value -} ->
  Crucible.TypeRepr ret    {- ^ type representation of function return value -} ->
  CrucibleMethodSpecIR     {- ^ specification for current function override  -} ->
  OverrideMatcher arch (Crucible.RegValue Sym ret)
methodSpecHandler1 opts sc cc args retTy cs =
    do expectedArgTypes <- (traverse . _1) resolveMemType (Map.elems (cs^.csArgBindings))

       sym <- getSymInterface

       let aux (memTy, setupVal) (Crucible.AnyValue tyrep val) =
             do storTy <- Crucible.toStorableType memTy
                pmv <- Crucible.packMemValue sym storTy tyrep val
                return (pmv, memTy, setupVal)

       -- todo: fail if list lengths mismatch
       xs <- liftIO (zipWithM aux expectedArgTypes (assignmentToList args))

       sequence_ [ matchArg sc cc PreState x y z | (x, y, z) <- xs]

       learnCond opts sc cc cs PreState (cs^.csPreState)

       executeCond opts sc cc cs (cs^.csPostState)

       computeReturnValue opts cc sc cs retTy (cs^.csRetValue)

-- learn pre/post condition
learnCond :: (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch))
          => Options
          -> SharedContext
          -> CrucibleContext arch
          -> CrucibleMethodSpecIR
          -> PrePost
          -> StateSpec
          -> OverrideMatcher arch ()
learnCond opts sc cc cs prepost ss = do
  matchPointsTos opts sc cc cs prepost (ss^.csPointsTos)
  traverse_ (learnSetupCondition opts sc cc cs prepost) (ss^.csConditions)
  enforceDisjointness cc ss
  enforceCompleteSubstitution ss


-- | Verify that all of the fresh variables for the given
-- state spec have been "learned". If not, throws
-- 'AmbiguousVars' exception.
enforceCompleteSubstitution :: StateSpec -> OverrideMatcher arch ()
enforceCompleteSubstitution ss =

  do sub <- OM (use termSub)

     let -- predicate matches terms that are not covered by the computed
         -- term substitution
         isMissing tt = termId (ttTerm tt) `Map.notMember` sub

         -- list of all terms not covered by substitution
         missing = filter isMissing (view csFreshVars ss)

     unless (null missing) (failure (AmbiguousVars missing))


-- | Given a 'Term' that must be an external constant, extract the 'VarIndex'.
termId :: Term -> VarIndex
termId t =
  case asExtCns t of
    Just ec -> ecVarIndex ec
    _       -> error "termId expected a variable"


-- execute a pre/post condition
executeCond :: (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch))
            => Options
            -> SharedContext
            -> CrucibleContext arch
            -> CrucibleMethodSpecIR
            -> StateSpec
            -> OverrideMatcher arch ()
executeCond opts sc cc cs ss = do
  refreshTerms sc ss

  ptrs <- liftIO $ Map.traverseWithKey
            (\k _memty -> executeFreshPointer cc k)
            (ss^.csFreshPointers)
  OM (setupValueSub %= Map.union ptrs)

  traverse_ (executeAllocation opts cc) (Map.assocs (ss^.csAllocs))
  traverse_ (executePointsTo opts sc cc cs) (ss^.csPointsTos)
  traverse_ (executeSetupCondition opts sc cc cs) (ss^.csConditions)


-- | Allocate fresh variables for all of the "fresh" vars
-- used in this phase and add them to the term substitution.
refreshTerms ::
  SharedContext {- ^ shared context -} ->
  StateSpec     {- ^ current phase spec -} ->
  OverrideMatcher arch ()
refreshTerms sc ss =
  do extension <- Map.fromList <$> traverse freshenTerm (view csFreshVars ss)
     OM (termSub %= Map.union extension)
  where
    freshenTerm tt =
      case asExtCns (ttTerm tt) of
        Just ec -> do new <- liftIO (mkTypedTerm sc =<< scFreshGlobal sc (ecName ec) (ecType ec))
                      return (termId (ttTerm tt), ttTerm new)
        Nothing -> error "refreshTerms: not a variable"

------------------------------------------------------------------------

-- | Generate assertions that all of the memory allocations matched by
-- an override's precondition are disjoint.
enforceDisjointness ::
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  CrucibleContext arch -> StateSpec -> OverrideMatcher arch ()
enforceDisjointness cc ss =
  do sym <- getSymInterface
     sub <- OM (use setupValueSub)
     let syms = Map.elems $ Map.intersectionWith (,) (view csAllocs ss) sub

     let resolve s = case TyCtx.asMemType s of
                       Nothing -> failRuntimeIO "enforceDisjointness: not memtype"
                       Just m  -> return m
     mems <- traverse (_1 resolve) syms

     sequence_
        [ do c <- liftIO
                $ Crucible.buildDisjointRegionsAssertion
                    sym Crucible.PtrWidth
                    p (sz pty)
                    q (sz qty)
             addAssert c a

        | let dl = TyCtx.llvmDataLayout (cc^.ccTypeCtx)

              sz p = Crucible.BVElt
                       Crucible.PtrWidth
                       (Crucible.bytesToInteger (Crucible.memTypeSize dl p))
                       Crucible.initializationLoc

              a = Crucible.AssertFailureSimError
                    "Memory regions not disjoint"

        , (pty,p):ps <- tails mems
        , (qty,q)    <- ps
        ]

------------------------------------------------------------------------

-- | For each points-to statement read the memory value through the
-- given pointer (lhs) and match the value against the given pattern
-- (rhs).  Statements are processed in dependency order: a points-to
-- statement cannot be executed until bindings for any/all lhs
-- variables exist.
matchPointsTos :: forall arch.
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  Options          {- ^ saw script print out opts -} ->
  SharedContext    {- ^ term construction context -} ->
  CrucibleContext arch {- ^ simulator context         -} ->
  CrucibleMethodSpecIR                               ->
  PrePost                                            ->
  [PointsTo]       {- ^ points-tos                -} ->
  OverrideMatcher arch ()
matchPointsTos opts sc cc spec prepost = go False []
  where
    go ::
      Bool       {- progress indicator -} ->
      [PointsTo] {- delayed conditions -} ->
      [PointsTo] {- queued conditions  -} ->
      OverrideMatcher arch ()

    -- all conditions processed, success
    go _ [] [] = return ()

    -- not all conditions processed, no progress, failure
    go False delayed [] = failure (AmbiguousPointsTos delayed)

    -- not all conditions processed, progress made, resume delayed conditions
    go True delayed [] = go False [] delayed

    -- progress the next points-to in the work queue
    go progress delayed (c:cs) =
      do ready <- checkPointsTo c
         if ready then
           do learnPointsTo opts sc cc spec prepost c
              go True delayed cs
         else
           do go progress (c:delayed) cs

    -- determine if a precondition is ready to be checked
    checkPointsTo :: PointsTo -> OverrideMatcher arch Bool
    checkPointsTo (PointsTo p _) = checkSetupValue p

    checkSetupValue :: SetupValue -> OverrideMatcher arch Bool
    checkSetupValue v =
      do m <- OM (use setupValueSub)
         return (all (`Map.member` m) (setupVars v))

    -- Compute the set of variable identifiers in a 'SetupValue'
    setupVars :: SetupValue -> Set AllocIndex
    setupVars v =
      case v of
        SetupVar    i  -> Set.singleton i
        SetupStruct xs -> foldMap setupVars xs
        SetupArray  xs -> foldMap setupVars xs
        SetupElem x _  -> setupVars x
        SetupField x _ -> setupVars x
        SetupTerm   _  -> Set.empty
        SetupNull      -> Set.empty
        SetupGlobal _  -> Set.empty


------------------------------------------------------------------------

-- | Compute the 'Crucible.MemType' for a given 'Crucible.SymType' or throw
-- an error.
resolveMemType ::
  (?lc :: TyCtx.LLVMContext) =>
  Crucible.SymType           ->
  OverrideMatcher arch Crucible.MemType
resolveMemType ty =
  case TyCtx.asMemType ty of
    Nothing    -> failure (BadSymType ty)
    Just memTy -> return memTy

------------------------------------------------------------------------

computeReturnValue ::
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  Options               {- ^ saw script debug and print options     -} ->
  CrucibleContext arch  {- ^ context of the crucible simulation     -} ->
  SharedContext         {- ^ context for generating saw terms       -} ->
  CrucibleMethodSpecIR  {- ^ method specification                   -} ->
  Crucible.TypeRepr ret {- ^ representation of function return type -} ->
  Maybe SetupValue      {- ^ optional symbolic return value         -} ->
  OverrideMatcher arch (Crucible.RegValue Sym ret)
                        {- ^ concrete return value                  -}

computeReturnValue _ _ _ _ ty Nothing =
  case ty of
    Crucible.UnitRepr -> return ()
    _ -> failure BadReturnSpecification

computeReturnValue opts cc sc spec ty (Just val) =
  do (_memTy, Crucible.AnyValue xty xval) <- resolveSetupValue opts cc sc spec val
     case Crucible.testEquality ty xty of
       Just Crucible.Refl -> return xval
       Nothing -> failure BadReturnSpecification


------------------------------------------------------------------------

-- | Forget the type indexes and length of the arguments.
assignmentToList ::
  Ctx.Assignment (Crucible.RegEntry sym) ctx ->
  [Crucible.AnyValue sym]
assignmentToList = Ctx.toListFC (\(Crucible.RegEntry x y) -> Crucible.AnyValue x y)

------------------------------------------------------------------------

getSymInterface :: OverrideMatcher arch Sym
getSymInterface = OM (use syminterface)

------------------------------------------------------------------------

-- | "Run" function for OverrideMatcher. The final result and state
-- are returned. The state will contain the updated globals and substitutions
runOverrideMatcher ::
   Sym                         {- ^ simulator                       -} ->
   Crucible.SymGlobalState Sym {- ^ initial global variables        -} ->
   Map AllocIndex (LLVMPtr (Crucible.ArchWidth arch)) {- ^ initial allocation substitution -} ->
   Map VarIndex Term           {- ^ initial term substitution       -} ->
   Set VarIndex                {- ^ initial free variables          -} ->
   OverrideMatcher arch a      {- ^ matching action                 -} ->
   IO (Either OverrideFailure (a, OverrideState arch))
runOverrideMatcher sym g a t free (OM m) = runExceptT (runStateT m (initialState sym g a t free))

------------------------------------------------------------------------

-- | Assign the given pointer value to the given allocation index in
-- the current substitution. If there is already a binding for this
-- index, then add a pointer-equality constraint.
assignVar ::
  Crucible.HasPtrWidth (Crucible.ArchWidth arch) =>
  CrucibleContext arch {- ^ context for interacting with Crucible -} ->
  AllocIndex      {- ^ variable index -} ->
  LLVMPtr (Crucible.ArchWidth arch) {- ^ concrete value -} ->
  OverrideMatcher arch ()

assignVar cc var val =
  do old <- OM (setupValueSub . at var <<.= Just val)
     for_ old $ \val' ->
       do p <- liftIO (equalValsPred cc (Crucible.ptrToPtrVal val') (Crucible.ptrToPtrVal val))
          addAssert p (Crucible.AssertFailureSimError "equality of aliased pointers")

------------------------------------------------------------------------


assignTerm ::
  SharedContext      {- ^ context for constructing SAW terms    -} ->
  CrucibleContext arch   {- ^ context for interacting with Crucible -} ->
  PrePost                                                          ->
  VarIndex {- ^ external constant index -} ->
  Term     {- ^ value                   -} ->
  OverrideMatcher arch ()

assignTerm sc cc prepost var val =
  do mb <- OM (use (termSub . at var))
     case mb of
       Nothing -> OM (termSub . at var ?= val)
       Just old ->
         matchTerm sc cc prepost val old

--          do t <- liftIO $ scEq sc old val
--             p <- liftIO $ resolveSAWPred cc t
--             addAssert p (Crucible.AssertFailureSimError ("literal equality " ++ stateCond prepost))


------------------------------------------------------------------------

-- | Match the value of a function argument with a symbolic 'SetupValue'.
matchArg ::
  Crucible.HasPtrWidth (Crucible.ArchWidth arch) =>
  SharedContext      {- ^ context for constructing SAW terms    -} ->
  CrucibleContext arch {- ^ context for interacting with Crucible -} ->
  PrePost                                                          ->
  Crucible.LLVMVal Sym
                     {- ^ concrete simulation value             -} ->
  Crucible.MemType   {- ^ expected memory type                  -} ->
  SetupValue         {- ^ expected specification value          -} ->
  OverrideMatcher arch ()

matchArg sc cc prepost actual expectedTy expected@(SetupTerm expectedTT)
  | Cryptol.Forall [] [] tyexpr <- ttSchema expectedTT
  , Right tval <- Cryptol.evalType mempty tyexpr
  = do sym      <- getSymInterface
       let failMsg = StructuralMismatch actual expected expectedTy
       realTerm <- valueToSC sym failMsg tval actual
       matchTerm sc cc prepost realTerm (ttTerm expectedTT)

-- match the fields of struct point-wise
matchArg sc cc prepost (Crucible.LLVMValStruct xs) (Crucible.StructType fields) (SetupStruct zs) =
  sequence_
    [ matchArg sc cc prepost x y z
       | ((_,x),y,z) <- zip3 (V.toList xs)
                             (V.toList (Crucible.fiType <$> Crucible.siFields fields))
                             zs ]

matchArg _sc cc prepost actual@(Crucible.LLVMValInt blk off) expectedTy setupval =
  case setupval of
    SetupVar var | Just Crucible.Refl <- Crucible.testEquality (Crucible.bvWidth off) Crucible.PtrWidth ->
      do assignVar cc var (Crucible.LLVMPointer blk off)

    SetupNull | Just Crucible.Refl <- Crucible.testEquality (Crucible.bvWidth off) Crucible.PtrWidth ->
      do sym <- getSymInterface
         p   <- liftIO (Crucible.ptrIsNull sym Crucible.PtrWidth (Crucible.LLVMPointer blk off))
         addAssert p (Crucible.AssertFailureSimError ("null-equality " ++ stateCond prepost))

    SetupGlobal name | Just Crucible.Refl <- Crucible.testEquality (Crucible.bvWidth off) Crucible.PtrWidth ->
      do let mem = cc^.ccEmptyMemImpl
         sym  <- getSymInterface
         Crucible.LLVMPointer blk' off' <- liftIO $ Crucible.doResolveGlobal sym mem (L.Symbol name)

         p1 <- liftIO (Crucible.natEq sym blk blk')
         p2 <- liftIO (Crucible.bvEq sym off off')
         p  <- liftIO (Crucible.andPred sym p1 p2)
         addAssert p (Crucible.AssertFailureSimError ("global-equality " ++ stateCond prepost))

    _ -> failure (StructuralMismatch actual setupval expectedTy)

matchArg _sc _cc _prepost actual expectedTy expected =
  failure (StructuralMismatch actual expected expectedTy)

------------------------------------------------------------------------

valueToSC ::
  Sym ->
  OverrideFailure ->
  Cryptol.TValue ->
  Crucible.LLVMVal Sym  ->
  OverrideMatcher arch Term
valueToSC sym failMsg (Cryptol.TVTuple tys) (Crucible.LLVMValStruct vals)
  | length tys == length vals
  = do terms <- traverse (\(ty, tm) -> valueToSC sym failMsg ty (snd tm)) (zip tys (V.toList vals))
       sc    <- liftIO (Crucible.saw_ctx <$> readIORef (Crucible.sbStateManager sym))
       liftIO $ scTuple sc terms

valueToSC sym failMsg (Cryptol.TVSeq _n Cryptol.TVBit) (Crucible.LLVMValInt base off) =
  do baseZero <- liftIO (Crucible.natEq sym base =<< Crucible.natLit sym 0)
     offTm    <- liftIO (Crucible.toSC sym off)
     case Crucible.asConstantPred baseZero of
       Just True  -> return offTm
       Just False -> failure failMsg
       _ -> do addAssert baseZero (Crucible.GenericSimError "Expected bitvector value, but found pointer")
               return offTm

-- This is a case for pointers, when we opaque types in Cryptol to represent them...
-- valueToSC sym _tval (Crucible.LLVMValInt base off) =
--   do base' <- Crucible.toSC sym base
--      off'  <- Crucible.toSC sym off
--      sc    <- Crucible.saw_ctx <$> readIORef (Crucible.sbStateManager sym)
--      Just <$> scTuple sc [base', off']

valueToSC sym failMsg (Cryptol.TVSeq n cryty) (Crucible.LLVMValArray ty vals)
  | toInteger (length vals) == n
  = do terms <- V.toList <$> traverse (valueToSC sym failMsg cryty) vals
       sc    <- liftIO (Crucible.saw_ctx <$> readIORef (Crucible.sbStateManager sym))
       t     <- liftIO (typeToSC sc ty)
       liftIO (scVector sc t terms)

valueToSC _ _ _ Crucible.LLVMValReal{} =
  failRuntimeIO  "valueToSC: Real not supported"

valueToSC _sym failMsg _tval _val =
  failure failMsg

------------------------------------------------------------------------

typeToSC :: SharedContext -> Crucible.Type -> IO Term
typeToSC sc t =
  case Crucible.typeF t of
    Crucible.Bitvector sz -> scBitvector sc (fromInteger (Crucible.bytesToBits sz))
    Crucible.Float -> failRuntimeIO "typeToSC: float not supported"
    Crucible.Double -> failRuntimeIO "typeToSC: double not supported"
    Crucible.Array sz ty ->
      do n <- scNat sc (fromIntegral sz)
         ty' <- typeToSC sc ty
         scVecType sc n ty'
    Crucible.Struct fields ->
      do fields' <- V.toList <$> traverse (typeToSC sc . view Crucible.fieldVal) fields
         scTuple sc fields'

------------------------------------------------------------------------

matchTerm ::
  SharedContext   {- ^ context for constructing SAW terms    -} ->
  CrucibleContext arch {- ^ context for interacting with Crucible -} ->
  PrePost                                                       ->
  Term            {- ^ exported concrete term                -} ->
  Term            {- ^ expected specification term           -} ->
  OverrideMatcher arch ()

matchTerm _ _ _ real expect | real == expect = return ()
matchTerm sc cc prepost real expect =
  do free <- OM (use osFree)
     case unwrapTermF expect of
       FTermF (ExtCns ec)
         | Set.member (ecVarIndex ec) free ->
         do assignTerm sc cc prepost (ecVarIndex ec) real

       _ ->
         do t <- liftIO $ scEq sc real expect
            p <- liftIO $ resolveSAWPred cc t
            addAssert p (Crucible.AssertFailureSimError ("literal equality " ++ stateCond prepost))

------------------------------------------------------------------------

-- | Use the current state to learn about variable assignments based on
-- preconditions for a procedure specification.
learnSetupCondition ::
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  Options                    ->
  SharedContext              ->
  CrucibleContext arch       ->
  CrucibleMethodSpecIR       ->
  PrePost                    ->
  SetupCondition             ->
  OverrideMatcher arch ()
learnSetupCondition opts sc cc spec prepost (SetupCond_Equal val1 val2)  = learnEqual opts sc cc spec prepost val1 val2
learnSetupCondition _opts sc cc _    prepost (SetupCond_Pred tm)         = learnPred sc cc prepost (ttTerm tm)
learnSetupCondition _opts sc cc _    prepost (SetupCond_Ghost var val)   = learnGhost sc cc prepost var val


------------------------------------------------------------------------

learnGhost ::
  SharedContext                                          ->
  CrucibleContext arch                                   ->
  PrePost                                                ->
  GhostGlobal                                            ->
  TypedTerm                                              ->
  OverrideMatcher arch ()
learnGhost sc cc prepost var expected =
  do actual <- readGlobal var
     matchTerm sc cc prepost (ttTerm actual) (ttTerm expected)

------------------------------------------------------------------------

-- | Process a "points_to" statement from the precondition section of
-- the CrucibleSetup block. First, load the value from the address
-- indicated by 'ptr', and then match it against the pattern 'val'.
learnPointsTo ::
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  Options                    ->
  SharedContext              ->
  CrucibleContext arch       ->
  CrucibleMethodSpecIR       ->
  PrePost                    ->
  PointsTo                   ->
  OverrideMatcher arch ()
learnPointsTo opts sc cc spec prepost (PointsTo ptr val) =
  do let tyenv = csAllocations spec
     memTy <- liftIO $ typeOfSetupValue cc tyenv val
     (_memTy, ptr1) <- asPointer =<< resolveSetupValue opts cc sc spec ptr
     -- In case the types are different (from crucible_points_to_untyped)
     -- then the load type should be determined by the rhs.
     storTy <- Crucible.toStorableType memTy
     sym    <- getSymInterface

     mem    <- readGlobal $ Crucible.llvmMemVar
                          $ (cc^.ccLLVMContext)

     res  <- liftIO (Crucible.loadRawWithCondition sym mem ptr1 storTy)
     (p,r,v) <- case res of
                  Left e  -> failure (BadPointerLoad e)
                  Right x -> return x
     addAssert p r
     matchArg sc cc prepost v memTy val


------------------------------------------------------------------------

stateCond :: PrePost -> String
stateCond PreState = "precondition"
stateCond PostState = "postcondition"

-- | Process a "crucible_equal" statement from the precondition
-- section of the CrucibleSetup block.
learnEqual ::
  Crucible.HasPtrWidth (Crucible.ArchWidth arch) =>
  Options                                          ->
  SharedContext                                    ->
  CrucibleContext arch                             ->
  CrucibleMethodSpecIR                             ->
  PrePost                                          ->
  SetupValue       {- ^ first value to compare  -} ->
  SetupValue       {- ^ second value to compare -} ->
  OverrideMatcher arch ()
learnEqual opts sc cc spec prepost v1 v2 = do
  (_, val1) <- resolveSetupValueLLVM opts cc sc spec v1
  (_, val2) <- resolveSetupValueLLVM opts cc sc spec v2
  p         <- liftIO (equalValsPred cc val1 val2)
  let name = "equality " ++ stateCond prepost
  addAssert p (Crucible.AssertFailureSimError name)

-- | Process a "crucible_precond" statement from the precondition
-- section of the CrucibleSetup block.
learnPred ::
  SharedContext                                                       ->
  CrucibleContext arch                                                ->
  PrePost                                                             ->
  Term             {- ^ the precondition to learn                  -} ->
  OverrideMatcher arch ()
learnPred sc cc prepost t =
  do s <- OM (use termSub)
     u <- liftIO $ scInstantiateExt sc s t
     p <- liftIO $ resolveSAWPred cc u
     addAssert p (Crucible.AssertFailureSimError (stateCond prepost))

------------------------------------------------------------------------

-- | Perform an allocation as indicated by a 'crucible_alloc'
-- statement from the postcondition section.
executeAllocation ::
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  Options                        ->
  CrucibleContext arch           ->
  (AllocIndex, Crucible.SymType) ->
  OverrideMatcher arch ()
executeAllocation opts cc (var, symTy) =
  do let sym = cc^.ccBackend
     let dl = TyCtx.llvmDataLayout ?lc
     memTy <- case TyCtx.asMemType symTy of
                Just memTy -> return memTy
                Nothing    -> failRuntimeIO "executAllocation: failed to resolve type"
     liftIO $ printOutLn opts Debug $ unwords ["executeAllocation:", show var, show memTy]
     let memVar = Crucible.llvmMemVar $ (cc^.ccLLVMContext)
     let w = Crucible.memTypeSize dl memTy
     mem <- readGlobal memVar
     sz <- liftIO $ Crucible.bvLit sym Crucible.PtrWidth (Crucible.bytesToInteger w)
     (ptr, mem') <- liftIO (Crucible.mallocRaw sym mem sz)
     writeGlobal memVar mem'
     assignVar cc var ptr

------------------------------------------------------------------------

-- | Update the simulator state based on the postconditions from the
-- procedure specification.
executeSetupCondition ::
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  Options                    ->
  SharedContext              ->
  CrucibleContext arch       ->
  CrucibleMethodSpecIR       ->
  SetupCondition             ->
  OverrideMatcher arch ()
executeSetupCondition opts sc cc spec (SetupCond_Equal val1 val2) = executeEqual opts sc cc spec val1 val2
executeSetupCondition _opts sc cc _    (SetupCond_Pred tm)        = executePred sc cc tm
executeSetupCondition _opts sc _  _    (SetupCond_Ghost var val)  = executeGhost sc var val

------------------------------------------------------------------------

executeGhost ::
  SharedContext ->
  GhostGlobal ->
  TypedTerm ->
  OverrideMatcher arch ()
executeGhost sc var val =
  do s <- OM (use termSub)
     t <- liftIO (ttTermLens (scInstantiateExt sc s) val)
     writeGlobal var t

------------------------------------------------------------------------

-- | Process a "points_to" statement from the postcondition section of
-- the CrucibleSetup block. First we compute the value indicated by
-- 'val', and then write it to the address indicated by 'ptr'.
executePointsTo ::
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  Options                    ->
  SharedContext              ->
  CrucibleContext arch       ->
  CrucibleMethodSpecIR       ->
  PointsTo                   ->
  OverrideMatcher arch ()
executePointsTo opts sc cc spec (PointsTo ptr val) =
  do (_, ptr1) <- asPointer =<< resolveSetupValue opts cc sc spec ptr
     sym    <- getSymInterface

     -- In case the types are different (from crucible_points_to_untyped)
     -- then the load type should be determined by the rhs.
     (memTy1, Crucible.AnyValue vtp val1) <- resolveSetupValue opts cc sc spec val
     storTy <- Crucible.toStorableType memTy1

     let memVar = Crucible.llvmMemVar $ (cc^.ccLLVMContext)
     mem  <- readGlobal memVar
     mem' <- liftIO (Crucible.doStore sym mem ptr1 vtp storTy val1)
     writeGlobal memVar mem'


------------------------------------------------------------------------


-- | Process a "crucible_equal" statement from the postcondition
-- section of the CrucibleSetup block.
executeEqual ::
  Crucible.HasPtrWidth (Crucible.ArchWidth arch) =>
  Options                                          ->
  SharedContext                                    ->
  CrucibleContext arch                             ->
  CrucibleMethodSpecIR                             ->
  SetupValue       {- ^ first value to compare  -} ->
  SetupValue       {- ^ second value to compare -} ->
  OverrideMatcher arch ()
executeEqual opts sc cc spec v1 v2 = do
  (_, val1) <- resolveSetupValueLLVM opts cc sc spec v1
  (_, val2) <- resolveSetupValueLLVM opts cc sc spec v2
  p         <- liftIO (equalValsPred cc val1 val2)
  addAssume p

-- | Process a "crucible_postcond" statement from the postcondition
-- section of the CrucibleSetup block.
executePred ::
  SharedContext     ->
  CrucibleContext arch ->
  TypedTerm        {- ^ the term to assert as a postcondition -} ->
  OverrideMatcher arch ()
executePred sc cc tt =
  do s <- OM (use termSub)
     t <- liftIO $ scInstantiateExt sc s (ttTerm tt)
     p <- liftIO $ resolveSAWPred cc t
     addAssume p

------------------------------------------------------------------------

-- | Construct a completely symbolic pointer. This pointer could point to anything, or it could
-- be NULL.
executeFreshPointer ::
  Crucible.HasPtrWidth (Crucible.ArchWidth arch) =>
  CrucibleContext arch {- ^ Crucible context       -} ->
  AllocIndex      {- ^ SetupVar allocation ID -} ->
  IO (LLVMPtr (Crucible.ArchWidth arch)) {- ^ Symbolic pointer value -}
executeFreshPointer cc (AllocIndex i) =
  do let mkName base = Crucible.systemSymbol (base ++ show i ++ "!")
         sym         = cc^.ccBackend
     blk <- Crucible.freshConstant sym (mkName "blk") Crucible.BaseNatRepr
     off <- Crucible.freshConstant sym (mkName "off") (Crucible.BaseBVRepr Crucible.PtrWidth)
     return (Crucible.LLVMPointer blk off)

------------------------------------------------------------------------

-- | Map the given substitution over all 'SetupTerm' constructors in
-- the given 'SetupValue'.
instantiateSetupValue ::
  SharedContext     ->
  Map VarIndex Term ->
  SetupValue        ->
  IO SetupValue
instantiateSetupValue sc s v =
  case v of
    SetupVar _     -> return v
    SetupTerm tt   -> SetupTerm <$> doTerm tt
    SetupStruct vs -> SetupStruct <$> mapM (instantiateSetupValue sc s) vs
    SetupArray  vs -> SetupArray <$> mapM (instantiateSetupValue sc s) vs
    SetupElem _ _  -> return v
    SetupField _ _ -> return v
    SetupNull      -> return v
    SetupGlobal _  -> return v
  where
    doTerm (TypedTerm schema t) = TypedTerm schema <$> scInstantiateExt sc s t

------------------------------------------------------------------------

resolveSetupValueLLVM ::
  Crucible.HasPtrWidth (Crucible.ArchWidth arch) =>
  Options              ->
  CrucibleContext arch ->
  SharedContext        ->
  CrucibleMethodSpecIR ->
  SetupValue           ->
  OverrideMatcher arch (Crucible.MemType, LLVMVal)
resolveSetupValueLLVM opts cc sc spec sval =
  do m <- OM (use setupValueSub)
     s <- OM (use termSub)
     let tyenv = csAllocations spec :: Map AllocIndex Crucible.SymType
     memTy <- liftIO $ typeOfSetupValue cc tyenv sval
     sval' <- liftIO $ instantiateSetupValue sc s sval
     lval  <- liftIO $ resolveSetupVal cc m tyenv sval' `X.catch` handleException opts
     return (memTy, lval)

resolveSetupValue ::
  Crucible.HasPtrWidth (Crucible.ArchWidth arch) =>
  Options              ->
  CrucibleContext arch ->
  SharedContext        ->
  CrucibleMethodSpecIR ->
  SetupValue           ->
  OverrideMatcher arch (Crucible.MemType, Crucible.AnyValue Sym)
resolveSetupValue opts cc sc spec sval =
  do (memTy, lval) <- resolveSetupValueLLVM opts cc sc spec sval
     sym <- getSymInterface
     aval <- liftIO $ Crucible.unpackMemValue sym lval
     return (memTy, aval)

------------------------------------------------------------------------

asPointer ::
  (?lc :: TyCtx.LLVMContext, Crucible.HasPtrWidth (Crucible.ArchWidth arch)) =>
  (Crucible.MemType, Crucible.AnyValue Sym) ->
  OverrideMatcher arch (Crucible.MemType, LLVMPtr (Crucible.ArchWidth arch))

asPointer
  (Crucible.PtrType pty,
   Crucible.AnyValue Crucible.PtrRepr val)
  | Just pty' <- TyCtx.asMemType pty
  = return (pty', val)

asPointer _ = failure BadPointerCast
