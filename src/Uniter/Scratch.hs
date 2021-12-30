module Uniter.Scratch () where

-- data Void1 a

-- data Pair a b = Pair !a !b
--   deriving stock (Eq, Show, Ord, Generic)
--   deriving anyclass (Hashable, NFData)

-- data Aligned f a b =
--     AlignedVars !a !b
--   | AlignedStructA !a !(f b)
--   | AlignedStructB !(f a) !b
--   deriving stock (Eq, Show)

-- alignedPair :: Pair a b -> Aligned f a b
-- alignedPair (Pair a b) = AlignedVars a b

-- newtype Embed f a = Embed { unEmbed :: f (Free f a) }
-- deriving stock instance Eq (f (Free f a)) => Eq (Embed f a)
-- deriving stock instance Show (f (Free f a)) => Show (Embed f a)

-- class Alignable m g f where
--   align :: f a -> f b -> LogicT m (g a b)

-- -- class AlignableAs m g f where
-- --   alignAs :: f a -> f b -> LogicT m (Aligned g a b)

-- -- instance Alignable m f => AlignableAs m Void1 f where
-- --   alignAs fa fb = fmap alignedPair (align fa fb)

-- instance Alignable m g f => Alignable m (Aligned (Embed f)) (Free f) where
--   align fa fb =
--     case fa of
--       FreePure a ->
--         case fb of
--           FreePure b -> pure (AlignedVars a b)
--           FreeEmbed tb -> pure (AlignedStructA a (Embed tb))
--       FreeEmbed ta ->
--         case fb of
--           FreePure b -> pure (AlignedStructB (Embed ta) b)
--           FreeEmbed tb -> alignAs (Embed ta) (Embed tb)

-- instance Alignable m g f => AlignableAs m (Embed f) (Embed f) where
--   alignAs (Embed ta) (Embed tb) = fixAlign (alignAs ta tb) where
--     fixAlign :: LogicT m (Aligned Void1 (Free f a) (Free f b)) -> LogicT m (Aligned (Embed f) a b)
--     fixAlign xs = xs >>= bindAlign
--     bindAlign :: Aligned Void1 (Free f a) (Free f b) -> LogicT m (Aligned (Embed f) a b)
--     bindAlign (AlignedVars fa fb) = alignAs fa fb

-- instance MonadFail m => Alignable m Pair TyF where
--   align ta tb =
--     case ta of
--       TyConstF ->
--         case tb of
--           TyConstF -> empty
--           _ -> fail "const error"
--       TyPairF fa1 fa2 ->
--         case tb of
--           TyConstF -> fail "pair error"
--           TyPairF fb1 fb2 -> pure (Pair fa1 fb1) <|> pure (Pair fa2 fb2)

-- newtype Part f = Part { unPart :: Free f BoundId }
-- deriving newtype instance Eq (f (Free f BoundId)) => Eq (Part f)
-- deriving stock instance Show (f (Free f BoundId)) => Show (Part f)

-- makePart :: (Recursive t, Base t ~ f) => t -> Part f
-- makePart = Part . go where
--   go = FreeEmbed . fmap go . project

-- newtype Node f = Node { unNode :: f BoundId }
-- deriving newtype instance Eq (f BoundId) => Eq (Node f)
-- deriving stock instance Show (f BoundId) => Show (Node f)

-- data Join f =
--     JoinRoot !(Node f)
--   | JoinLeaf !BoundId
-- deriving stock instance Eq (f BoundId) => Eq (Join f)
-- deriving stock instance Show (f BoundId) => Show (Join f)

-- newtype BoundEnv f = BoundEnv { unBoundEnv :: Map BoundId (Join f) }
-- deriving newtype instance Eq (f BoundId) => Eq (BoundEnv f)
-- deriving stock instance Show (f BoundId) => Show (BoundEnv f)

-- lookupWholeVar :: (Corecursive t, Base t ~ f, Traversable f) => BoundEnv f -> BoundId -> Either BoundId t
-- lookupWholeVar b@(BoundEnv m) v =
--   case Map.lookup v m of
--     Nothing -> Left v
--     Just (Part w) -> lookupWholeFree b w

-- lookupWholePart :: (Corecursive t, Base t ~ f, Traversable f) => BoundEnv f -> Part f -> Either BoundId t
-- lookupWholePart b = lookupWholeFree b . unPart

-- lookupWholeFree :: (Corecursive t, Base t ~ f, Traversable f) => BoundEnv f -> Free f BoundId -> Either BoundId t
-- lookupWholeFree b w =
--   case w of
--     FreePure v -> lookupWholeVar b v
--     FreeEmbed ff -> fmap embed (traverse (lookupWholeFree b) ff)


-- type BoundEq = Pair BoundId BoundId

-- data St f = St
--   { stUnique :: !BoundId
--   , stFreeEnv :: !FreeEnv
--   , stBoundEnv :: !(BoundEnv f)
--   , stBoundEqs :: !(Seq BoundEq)
--   }
-- deriving stock instance Eq (f BoundId) => Eq (St f)
-- deriving stock instance Show (f BoundId) => Show (St f)

-- newtype M e f a = M { unM :: StateT (St f) (Except (UniterError e)) a }
--   deriving newtype (Functor, Applicative, Monad, MonadState (St f), MonadError (UniterError e))

-- runM :: M e f a -> St f -> Either (UniterError e) (a, St f)
-- runM m st = runExcept (runStateT (unM m) st)

-- class Substitutable f x | x -> f where
--   subReplace :: BoundEnv f -> x -> x
--   subBoundVars :: x -> [BoundId]

-- instance (Functor f, Foldable f) => Substitutable f (Join f) where
  -- subReplace (BoundEnv m)
  -- subBoundVars = toList . unNode

-- instance (Functor f, Foldable f) => Substitutable f (Part f) where
--   subReplace (BoundEnv m) = Part . go . unPart where
--     go z =
--       case z of
--         FreePure v -> maybe z unPart (Map.lookup v m)
--         FreeEmbed fx -> FreeEmbed (fmap go fx)
--   subBoundVars = go . unPart where
--     go = \case
--       FreePure v -> [v]
--       FreeEmbed fx -> foldMap go fx

-- instance (Functor f, Foldable f) => Substitutable f (BoundEnv f) where
--   subReplace w@(BoundEnv s) (BoundEnv t) = BoundEnv (Map.union s (fmap (subReplace w) t))
--   subBoundVars = foldMap subBoundVars . unBoundEnv

-- newVarM :: M e f BoundId
-- newVarM = state $ \st ->
--   let fresh = stUnique st
--       st' = st { stUnique = succ fresh }
--   in (fresh, st')

-- addPartM :: Traversable f => Part f -> M e f BoundId
-- addPartM = addFreeM . unPart

-- addFreeM :: Traversable f => Free f BoundId -> M e f BoundId
-- addFreeM w =
--   case w of
--     FreePure v -> pure v
--     FreeEmbed ff -> do
--       fv <- traverse addFreeM ff
--       addNodeM (Node fv)

-- addNodeM :: Node f -> M e f BoundId
-- addNodeM n = do
--     v <- newVarM
--     modify' (\st -> st { stBoundEnv = BoundEnv (Map.insert v (JoinRoot n) (unBoundEnv (stBoundEnv st))) })
--     pure v

-- emitEqM :: BoundId -> BoundId -> M e f ()
-- emitEqM i j = modify' (\st -> st { stBoundEqs = stBoundEqs st :|> Pair i j })

-- replaceM :: (Functor f, Foldable f) => Part f -> M e f (Part f)
-- replaceM v = do
--   m <- gets stBoundEnv
--   pure (subReplace m v)

-- valUnifyM :: Val -> Val -> M Val
-- valUnifyM = go where
--   go v w =
--     case v of
--       ValConst ->
--         case w of
--           ValConst -> pure v
--           _ -> pure (ValErr "fail")

-- lookupFreeEnvM :: FreeName -> M e f (Maybe BoundId)
-- lookupFreeEnvM x = gets (Map.lookup x . unFreeEnv . stFreeEnv)

-- assignFreeEnvM :: FreeName -> BoundId -> M e f a -> M e f a
-- assignFreeEnvM x i = localFreeEnvM (FreeEnv . Map.insert x i . unFreeEnv)

-- localFreeEnvM :: (FreeEnv -> FreeEnv) -> M e f a -> M e f a
-- localFreeEnvM f act = do
--   freeEnv <- state (\st -> let x = stFreeEnv st in (x, st { stFreeEnv = f x }))
--   ret <- act
--   state (\st -> (ret, st { stFreeEnv = freeEnv }))


-- interpM :: UniterM e f a -> M e f a
-- interpM = subInterpM . unUniterM

-- subInterpM :: Free (UniterF e f (UniterM e f)) a -> M e f a
-- subInterpM = \case
--   FreePure a -> pure a
--   FreeEmbed u ->
--     case u of
--       UniterThrowError e -> throwError e
--       UniterLookupFree i k -> lookupFreeEnvM i >>= subInterpM . k
--       UniterAssignFree x i m -> assignFreeEnvM x i (interpM m >>= subInterpM)
--       UniterEmitEq i j k -> emitEqM i j >> subInterpM k
--       UniterAddNode n k -> addNodeM n >>= subInterpM . k
