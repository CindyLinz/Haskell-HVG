module Graph where

import Data.Traversable
import Control.Monad

import HVG.Type
import HVG.Context2D
import HVG.ContextState

import Entity

graph :: Builder Link ContextState Draw ()
graph = do
  {-
  box 10 10 100 590 3
    [ text "Hi"
    , text "QQ"
    , box 10 10 80 60 2 []
    ]
  -}
  link "TEntry 1-1" "TVar 1"
  box 450 10 100 590 1 $ pure $ do
    textTop "world"

    let
      values = ["1", "3", "'a'", "-4,2", "'xx'", "3.5", "X"]

    forM_ (zip [1..] values) $ \(i, val) -> do
      name ("TVar " ++ show i)
      box 10 (40 + fromIntegral (i - 1) * 70) 80 60 2
        [ text ("TVar " ++ show i)
        , text val
        ]

  box 30 15 100 150 1 $ pure $ do
    textTop "TRec 1"

    name "TEntry 1-1"
    box 10 40 80 90 3 $
      [ text "TRecEntry"
      , text "1"
      , text "3"
      ]

    --link "TEntry 1-1" "TVar 1"

  name "TRec 2-1"
  box 200 150 100 240 1 $ pure $ do
    textTop "TRec 2-1"

    name "TEntry 2-1-1"
    box 10 40 80 90 3
      [ text "TRecEntry"
      , text "2"
      , text "3"
      ]

    link "TEntry 2-1-1" "TVar 2"

    name "TEntry 2-1-2"
    box 10 140 80 90 3
      [ text "TRecEntry"
      , text "'a'"
      , text "'a'"
      ]

    link "TEntry 2-1-2" "TVar 3"

  link "TRec 2-1" "TRec 2-2"

  name "TRec 2-2"
  box 200 400 100 150 1 $ pure $ do
    textTop "TRec 2-2"

    name "TEntry 2-2-1"
    box 10 40 80 90 3
      [ text "TRecEntry"
      , text "'a'"
      , text "'b'"
      ]

  name "inv 1"
  ellipse 750 100 80 40 $ do
    text "inv 1"

  link "inv 1" "TRec inv 1"
  link "TRec inv 1" "TVar 2"
  link "TRec inv 1" "TVar 3"

  name "TRec inv 1"
  box 600 120 100 40 1 $ pure $ do
    textTop "TRec inv 1"

  box 850 250 100 300 1 $ pure $ do
    textTop "TRec 3"

    name "inv 2"
    ellipse 10 50 80 40 $ do
      text "inv 2"

  link "inv 2" "TRec inv 2"

  name "TRec inv 2"
  box 700 320 100 40 1 $ pure $ do
    textTop "TRec inv 2"

  link "TRec inv 2" "TVar 3"
  link "TRec inv 2" "TVar 5"
  link "TRec inv 2" "TVar 6"
