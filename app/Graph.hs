module Graph where

import Data.Traversable
import Control.Monad

import HVG.Type
import HVG.Context2D
import HVG.ContextState

import Entity

graph :: Builder ()
graph = do
  {-
  box 10 10 100 590 3
    [ text "Hi"
    , text "QQ"
    , box 10 10 80 60 2 []
    ]
  -}
  link "TVar5" "TEntry1-1"

  box 450 10 100 590 1 $ pure $ do
    let
      values = ["1", "3", "'a'", "-4,2", "'xx'", "3.5", "X"]

    textTop "world"

    forM_ (zip [1..] values) $ \(i, val) -> do
      name ("TVar" ++ show i)
      box 10 (40 - 70 + fromIntegral i * 70) 80 60 2 $
        [ text ("TVar " ++ show i)
        , text val
        ]

  link "link1" "link2"

  local $ do
    name "link1"
    link "TVar1" "TEntry1-1"

  box 30 15 100 150 1 $ pure $ do
    textTop "TRec 1"

    name "TEntry1-1"
    box 10 40 80 90 3 $
      [ text "TRecEntry"
      , text "1"
      , text "3"
      ]

  name "link2"
  link "TVar3" "TEntry1-1"
