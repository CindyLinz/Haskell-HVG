module HVG.Type where

import Data.Monoid
import qualified Data.Map.Strict as M

data BuilderState info ctx draw = BuilderState
  { bldNamedInfo :: M.Map String info
  , bldWaitInfo :: M.Map String [ContextedWaitInfoBuilder info ctx draw ()]
  , bldDraw :: draw
  }
initBuilderState :: Monoid draw => BuilderState info ctx draw
initBuilderState = BuilderState
  { bldNamedInfo = M.empty
  , bldWaitInfo = M.empty
  , bldDraw = mempty
  }

addBuilderWaitInfo :: String -> ContextedWaitInfoBuilder info ctx draw () -> BuilderState info ctx draw -> BuilderState info ctx draw
addBuilderWaitInfo infoName ctxdBld bld = bld
  { bldWaitInfo = M.insertWith (++) infoName [ctxdBld] (bldWaitInfo bld)
  }

data BuilderPart info ctx draw a
  = BuilderPartDone (Maybe String) ctx (BuilderState info ctx draw) a
  | BuilderPartWaitInfo String (BuilderState info ctx draw) (ContextedWaitInfoBuilder info ctx draw a)

mapBuilderPart :: (Maybe String -> ctx -> BuilderState info ctx draw -> a -> BuilderPart info ctx draw b) -> BuilderPart info ctx draw a -> BuilderPart info ctx draw b
mapBuilderPart f = go
  where
  go = \case
    BuilderPartDone nextName ctx bld a -> f nextName ctx bld a
    BuilderPartWaitInfo infoName bld (ContextedWaitInfoBuilder ctxdAAct) ->
      BuilderPartWaitInfo infoName bld $ ContextedWaitInfoBuilder $ \link bld' -> go (ctxdAAct link bld')

forBuilderPart :: BuilderPart info ctx draw a -> (Maybe String -> ctx -> BuilderState info ctx draw -> a -> BuilderPart info ctx draw b) -> BuilderPart info ctx draw b
forBuilderPart = flip mapBuilderPart

suspendBuilderPartWait :: BuilderPart info ctx draw () -> BuilderState info ctx draw
suspendBuilderPartWait = \case
  BuilderPartDone _ _ bld' _ ->
    bld'
  BuilderPartWaitInfo infoName bld' ctxdBld ->
    addBuilderWaitInfo infoName ctxdBld bld'

newtype Builder info ctx draw a = Builder (Maybe String -> ctx -> BuilderState info ctx draw -> BuilderPart info ctx draw a)
newtype ContextedWaitInfoBuilder info ctx draw a = ContextedWaitInfoBuilder (info -> BuilderState info ctx draw -> BuilderPart info ctx draw a)

fork :: Builder info ctx draw () -> Builder info ctx draw ()
fork (Builder act) = Builder $ \nextName ctx bld ->
  BuilderPartDone
    Nothing
    ctx
    (suspendBuilderPartWait (act nextName ctx bld))
    ()

local :: Builder info ctx draw a -> Builder info ctx draw a
local (Builder act) = Builder $ \nextName ctx bld ->
  forBuilderPart (act nextName ctx bld) $ \_ _ bld' a ->
    BuilderPartDone Nothing ctx bld' a

instance Functor (Builder info ctx draw) where
  fmap f (Builder act) = Builder $ \nextName ctx bld ->
    fmap f (act nextName ctx bld)
instance Functor (ContextedWaitInfoBuilder info ctx draw) where
  fmap f (ContextedWaitInfoBuilder act) = ContextedWaitInfoBuilder $ \link bld ->
    fmap f (act link bld)
instance Functor (BuilderPart info ctx draw) where
  fmap f = \case
    BuilderPartDone nextName ctx bld a -> BuilderPartDone nextName ctx bld (f a)
    BuilderPartWaitInfo infoName bld ctxdBuilder -> BuilderPartWaitInfo infoName bld (fmap f ctxdBuilder)

instance Applicative (Builder info ctx draw) where
  pure a = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld a
  Builder fAct <*> Builder aAct = Builder $ \nextName ctx bld ->
    forBuilderPart (fAct nextName ctx bld) $ \nextName' ctx' bld' f ->
      f <$> aAct nextName' ctx' bld'

instance Monad (Builder info ctx draw) where
  Builder mAct >>= f = Builder $ \nextName ctx bld ->
    forBuilderPart (mAct nextName ctx bld) $ \nextName' ctx' bld' a ->
      let
        Builder fAct = f a
      in
        fAct nextName' ctx' bld'

name :: String -> Builder info ctx draw ()
name nextName = Builder $ \nextName' ctx bld ->
  BuilderPartDone
    (Just nextName)
    ctx
    bld
    ()

addDraw :: Monoid draw => draw -> Builder info ctx draw ()
addDraw draw = Builder $ \nextName ctx bld ->
  BuilderPartDone
    nextName
    ctx
    bld{ bldDraw = bldDraw bld <> draw }
    ()

addInfo :: info -> Builder info ctx draw ()
addInfo info = Builder $ \nextName ctx bld ->
  case nextName of
    Nothing ->
      BuilderPartDone
        nextName
        ctx
        bld
        ()

    Just myName ->
      let
        bld' = bld
          { bldNamedInfo = M.insert myName info (bldNamedInfo bld)
          , bldWaitInfo = M.delete myName (bldWaitInfo bld)
          }

        bld'' = case M.lookup myName (bldWaitInfo bld) of
          Nothing ->
            bld'
          Just ctxdBlds ->
            go bld' ctxdBlds
            where
              go bld' (ContextedWaitInfoBuilder continue : otherCtxdBlds) =
                go (suspendBuilderPartWait (continue info bld')) otherCtxdBlds

              go bld' _ =
                bld'

      in
        BuilderPartDone Nothing ctx bld'' ()

queryInfo :: String -> Builder info ctx draw info
queryInfo infoName = Builder $ \nextName ctx bld ->
  case M.lookup infoName (bldNamedInfo bld) of
    Just info ->
      BuilderPartDone nextName ctx bld info
    Nothing ->
      --error $ infoName ++ " " ++ show (map fst $ M.toList $ bldNamedInfo bld)
      BuilderPartWaitInfo infoName bld $ ContextedWaitInfoBuilder $ \info bld' ->
        BuilderPartDone nextName ctx bld' info

