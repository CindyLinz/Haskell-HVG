module HVG.Type where

import Data.Monoid
import qualified Data.Map.Strict as M
import Data.IterLinkedList as L

data AgingBuilder agingState = AgingBuilder
  { agingBuilderIter :: Integer
  , agingBuilderSegs :: (L.LinkedList Integer agingState)
  }

data Builder info structState agingState a = Builder
  { unBuilder ::
    ( Maybe String -- 命名中的名字
   -> structState
   -> BuilderPart info structState agingState a
    )
  }

data SuspendedBuilder info structState agingState a = SuspendedBuilder
  { unSuspendedBuilder ::
    ( info
   -> BuilderPart info structState agingState a
    )
  }

data BuilderPart info structState agingState a = BuilderPart
  { unBuilderPart ::
    ( AgingBuilder agingState
   -> M.Map String info -- 取了名字的資訊
   -> M.Map String [SuspendedBuilder info structState agingState ()]
   -> BuilderMode info structState agingState a
    )
  }

data BuilderMode info structState agingState a
  = BuilderExec
    (Maybe String)
    structState
    (AgingBuilder agingState)
    (M.Map String info)
    (M.Map String [SuspendedBuilder info structState agingState ()])
    a
  | BuilderWait
    String -- 正在等待的名字
    (AgingBuilder agingState) -- 留好暫停部位的洞, 以及指定可以先繼續畫的部位
    (M.Map String info)
    (M.Map String [SuspendedBuilder info structState agingState ()])
    (SuspendedBuilder info structState agingState a)

runBuilder :: Monoid agingState => Builder info structState agingState a -> structState -> (a, (agingState, [String]))
runBuilder (Builder m) structState =
  let
    initAgingSegs = L.singleton mempty
    initAgingState = AgingBuilder (L.lastIter initAgingSegs) initAgingSegs
  in case unBuilderPart (m Nothing structState) initAgingState M.empty M.empty of
    BuilderExec maybeName' structState' agingState' namedInfo' suspendedBuilder' a ->
      (a, (mconcat (L.toList (agingBuilderSegs agingState')), M.keys suspendedBuilder'))
    BuilderWait needName agingState' namedInfo' suspendedBuilder' susp ->
      (undefined, (mconcat (L.toList (agingBuilderSegs agingState')), M.keys suspendedBuilder'))

evalBuilder :: Monoid agingState => Builder info structState agingState a -> structState -> a
evalBuilder builder structState = fst $ runBuilder builder structState

execBuilder :: Monoid agingState => Builder info structState agingState a -> structState -> (agingState, [String])
execBuilder builder structState = snd $ runBuilder builder structState

instance Functor (Builder info structState agingState) where
  fmap f (Builder m) = Builder $ \maybeName structState ->
    fmap f (m maybeName structState)

instance Functor (BuilderPart info structState agingState) where
  fmap f (BuilderPart m) = BuilderPart $ \agingState namedInfo suspendedBuilder ->
    fmap f (m agingState namedInfo suspendedBuilder)

instance Functor (BuilderMode info structState agingState) where
  fmap f (BuilderExec maybeName structState agingState namedInfo suspendedBuilder a) =
    BuilderExec maybeName structState agingState namedInfo suspendedBuilder (f a)
  fmap f (BuilderWait needName agingState namedInfo suspendedBuilder susp) =
    BuilderWait needName agingState namedInfo suspendedBuilder (fmap f susp)

instance Functor (SuspendedBuilder info structState agingState) where
  fmap f (SuspendedBuilder susp) = SuspendedBuilder $ \info ->
    fmap f (susp info)


instance Applicative (Builder info structState agingState) where
  pure a = Builder $ \maybeName structState ->
    BuilderPart $ \agingState namedInfo suspendedBuilder ->
      BuilderExec maybeName structState agingState namedInfo suspendedBuilder a

  Builder f <*> Builder a = Builder $ \maybeName structState ->
    forBuilderPart (f maybeName structState) $
      \maybeName' structState' agingState' namedInfo' suspendedBuilder' g ->
        let
          BuilderPart fPart = forBuilderPart (a maybeName' structState') $
            \maybeName'' structState'' agingState'' namedInfo'' suspendedBuilder'' a ->
              BuilderExec maybeName'' structState'' agingState'' namedInfo'' suspendedBuilder'' (g a)
        in
          fPart agingState' namedInfo' suspendedBuilder'


instance Monad (Builder info structState agingState) where
  Builder m >>= f = Builder $ \maybeName structState ->
    forBuilderPart (m maybeName structState) $
      \maybeName' structState' agingState' namedInfo' suspendedBuilder' a ->
        let
          Builder g = f a
          BuilderPart gPart = g maybeName' structState'
        in
          gPart agingState' namedInfo' suspendedBuilder'


mapBuilderPart
  :: ( Maybe String -> structState -> AgingBuilder agingState -> M.Map String info
    -> M.Map String [SuspendedBuilder info structState agingState ()]
    -> a -- 此函數的參數部分就是 BuilderExec 裡會拿到的各項
    -> BuilderMode info structState agingState b
     )
  -> BuilderPart info structState agingState a
  -> BuilderPart info structState agingState b
mapBuilderPart f (BuilderPart mPart) =
  BuilderPart $ \agingState namedInfo suspendedBuilder ->
    go (mPart agingState namedInfo suspendedBuilder)
    where

      go ( BuilderExec
           maybeName structState agingState
           namedInfo suspendedBuilder a
         ) = f maybeName structState agingState namedInfo suspendedBuilder a

      go ( BuilderWait
           needName agingState
           namedInfo suspendedBuilder (SuspendedBuilder susp)
         ) = BuilderWait needName agingState namedInfo suspendedBuilder $
           SuspendedBuilder $ \info ->
             BuilderPart $ \agingState' namedInfo' suspendedBuilder' ->
               let BuilderPart suspPart = susp info
               in go (suspPart agingState' namedInfo' suspendedBuilder')

forBuilderPart = flip mapBuilderPart

local (Builder m) = Builder $ \maybeName structState ->
  forBuilderPart (m maybeName structState) $
    \maybeName' structState' agingState' namedInfo' suspendedBuilder' a ->
      BuilderExec
        maybeName' structState {- 用舊的 structState -}
        agingState' namedInfo' suspendedBuilder' a

name :: String -> Builder info structState agingState ()
name nextName = Builder $ \ignoredMaybeName structState ->
  BuilderPart $ \agingState namedInfo suspendedBuilder ->
    BuilderExec (Just nextName) structState agingState namedInfo suspendedBuilder ()

addInfo :: info -> Builder info structState agingState ()
addInfo info =
  Builder $ \maybeName structState ->
    BuilderPart $ \agingState namedInfo suspendedBuilder ->
      case maybeName of
        Nothing -> BuilderExec maybeName structState
          agingState namedInfo suspendedBuilder ()
        Just name ->
          BuilderExec Nothing structState agingState'' namedInfo'' suspendedBuilder'' ()
            where
            namedInfo' = M.insert name info namedInfo
            (agingState'', namedInfo'', suspendedBuilder'') =
              case M.lookup name suspendedBuilder of
                Nothing -> (agingState, namedInfo', suspendedBuilder)
                Just susps -> go agingState namedInfo' (M.delete name suspendedBuilder) susps
                  where
                    AgingBuilder iter _ = agingState
                    go agingState namedInfo suspendedBuilder susps =
                      case susps of
                        [] -> (AgingBuilder iter agingSegs, namedInfo, suspendedBuilder)
                          where
                            AgingBuilder _ agingSegs = agingState
                        SuspendedBuilder susp : susps ->
                          let BuilderPart sPart = susp info
                          in case sPart agingState namedInfo suspendedBuilder of

                            -- 拿了這個 info 就滿足了
                            BuilderExec maybeName' structState' agingState'
                                namedInfo' suspendedBuilder' _ ->
                              go agingState' namedInfo' suspendedBuilder' susps

                            -- 拿了這個 info 以後又遇到別的缺乏的 info
                            BuilderWait needName agingState'
                                namedInfo' suspendedBuilder' susp' ->
                              go agingState' namedInfo'
                                (M.insertWith (++) needName [susp'] suspendedBuilder') susps

queryInfo :: Monoid agingState => String -> Builder info structState agingState info
queryInfo name =
  Builder $ \maybeName structState ->
    BuilderPart $ \agingState namedInfo suspendedBuilder->

      case M.lookup name namedInfo of
        Just info ->
          BuilderExec maybeName structState
            agingState namedInfo suspendedBuilder info

        Nothing ->
          BuilderWait name agingStateNewHole namedInfo suspendedBuilder $
            SuspendedBuilder $ \info ->
              BuilderPart $ \(AgingBuilder _ agingSegs') namedInfo' suspendedBuilder' ->
                BuilderExec maybeName structState
                  (AgingBuilder iter agingSegs') namedInfo' suspendedBuilder' info
          where
            AgingBuilder iter agingSegs = agingState
            agingSegsNewHole = L.insertAfter iter mempty agingSegs
            agingStateNewHole = AgingBuilder (L.next agingSegsNewHole iter) agingSegsNewHole

fork
  :: Builder info structState agingState ()
    -- 或是 Builder info structState agingState a 也可以
    -- 不過放在那裡的東西無論是什麼, 註定是要作廢的 [1]
  -> Builder info structState agingState ()
fork (Builder m) =
  Builder $ \maybeName structState ->
    BuilderPart $ \agingState namedInfo suspendedBuilder ->
      let
        BuilderPart mPart = m maybeName structState
      in
        case mPart agingState namedInfo suspendedBuilder of
          BuilderWait needName agingState' namedInfo' suspendedBuilder' susp ->
            BuilderExec
              Nothing structState agingState' namedInfo'
              (M.insertWith (++) needName [susp] suspendedBuilder') ()
              -- 如果 [1] 處是 a 而不是 () 的話,
              -- 這邊要加一個把 susp 的 a 丟棄換成 () 的動作
          BuilderExec maybeName' structState'
              agingState' namedInfo' suspendedBuilder' _ ->
            BuilderExec
              Nothing structState agingState' namedInfo'
              suspendedBuilder' ()

getStructState :: Builder info structState agingState structState
getStructState = Builder $ \maybeName structState ->
  BuilderPart $ \agingState namedInfo suspendedBuilder ->
    BuilderExec maybeName structState agingState namedInfo suspendedBuilder structState

putStructState :: structState -> Builder info structState agingState ()
putStructState structState = Builder $ \maybeName _ ->
  BuilderPart $ \agingState namedInfo suspendedBuilder ->
    BuilderExec maybeName structState agingState namedInfo suspendedBuilder ()

modifyStructState :: (structState -> structState) -> Builder info structState agingState ()
modifyStructState f = Builder $ \maybeName structState ->
  BuilderPart $ \agingState namedInfo suspendedBuilder ->
    BuilderExec maybeName (f structState) agingState namedInfo suspendedBuilder ()

appendAgingState :: Monoid agingState => agingState -> Builder info structState agingState ()
appendAgingState agingState = Builder $ \maybeName structState ->
  BuilderPart $ \(AgingBuilder agingIter agingSegs) namedInfo suspendedBuilder ->
    BuilderExec maybeName structState
      (AgingBuilder agingIter (L.modify agingIter (<> agingState) agingSegs))
      namedInfo suspendedBuilder ()
