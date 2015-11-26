module Spaceship where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import Keyboard
import Window
import Time

main : Signal Element
main =
  Signal.map show Window.dimensions
