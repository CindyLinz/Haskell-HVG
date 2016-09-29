module HVG.Type where

import Data.Monoid
import qualified Data.Map.Strict as M

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
    ( agingState
   -> M.Map String info -- 取了名字的資訊
   -> M.Map String [SuspendedBuilder info structState agingState ()]
   -> BuilderMode info structState agingState a
    )
  }

data BuilderMode info structState agingState a
  = BuilderExec
    (Maybe String)
    structState
    agingState
    (M.Map String info)
    (M.Map String [SuspendedBuilder info structState agingState ()])
    a
  | BuilderWait
    String -- 正在等待的名字
    agingState -- 暫停以前已經可以畫的部分
    (M.Map String info)
    (M.Map String [SuspendedBuilder info structState agingState ()])
    (SuspendedBuilder info structState agingState a)

runBuilder :: Builder info structState agingState a -> structState -> agingState -> (a, (agingState, [String]))
runBuilder (Builder m) structState agingState =
  case unBuilderPart (m Nothing structState) agingState M.empty M.empty of
    BuilderExec maybeName' structState' agingState' namedInfo' suspendedBuilder' a ->
      (a, (agingState', M.keys suspendedBuilder'))
    BuilderWait needName agingState' namedInfo' suspendedBuilder' susp ->
      (undefined, (agingState', M.keys suspendedBuilder'))

evalBuilder :: Builder info structState agingState a -> structState -> agingState -> a
evalBuilder builder structState agingState = fst $ runBuilder builder structState agingState

execBuilder :: Builder info structState agingState a -> structState -> agingState -> (agingState, [String])
execBuilder builder structState agingState = snd $ runBuilder builder structState agingState

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
  :: ( Maybe String -> structState -> agingState -> M.Map String info
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
                Just susps -> go agingState namedInfo'
                  (M.delete name suspendedBuilder) susps
                  where
                    go agingState namedInfo suspendedBuilder susps =
                      case susps of
                        [] -> (agingState, namedInfo, suspendedBuilder)
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

queryInfo :: String -> Builder info structState agingState info
queryInfo name =
  Builder $ \maybeName structState ->
    BuilderPart $ \agingState namedInfo suspendedBuilder->

      case M.lookup name namedInfo of
        Just info ->
          BuilderExec maybeName structState
            agingState namedInfo suspendedBuilder info

        Nothing ->
          BuilderWait name agingState namedInfo suspendedBuilder $
            SuspendedBuilder $ \info ->
              BuilderPart $ \agingState' namedInfo' suspendedBuilder' ->
                BuilderExec maybeName structState
                  agingState' namedInfo' suspendedBuilder' info

fork
  :: Builder info structState agingState ()
    -- 或是 Builder info structState agingState a 也可以
    -- 不過放在那裡的東西無論是什麼, 注定是要作廢的 [1]
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

getAgingState :: Builder info structState agingState agingState
getAgingState = Builder $ \maybeName structState ->
  BuilderPart $ \agingState namedInfo suspendedBuilder ->
    BuilderExec maybeName structState agingState namedInfo suspendedBuilder agingState

putAgingState :: agingState -> Builder info structState agingState ()
putAgingState agingState = Builder $ \maybeName structState ->
  BuilderPart $ \_ namedInfo suspendedBuilder ->
    BuilderExec maybeName structState agingState namedInfo suspendedBuilder ()

modifyAgingState :: (agingState -> agingState) -> Builder info structState agingState ()
modifyAgingState f = Builder $ \maybeName structState ->
  BuilderPart $ \agingState namedInfo suspendedBuilder ->
    BuilderExec maybeName structState (f agingState) namedInfo suspendedBuilder ()

appendAgingState :: Monoid agingState => agingState -> Builder info structState agingState ()
appendAgingState agingState = modifyAgingState (<> agingState)
