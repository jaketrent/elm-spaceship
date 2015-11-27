module Spaceship where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import Keyboard
import Window
import Time


-- MODEL

type alias Model =
  { position: Int,
    powerLevel: Int,
    isFiring: Bool
  }

initialShip : Model
initialShip =
  { position = 0,
    powerLevel = 10,
    isFiring = False
  }


-- UPDATE

type Action = NoOp | Move Int | Fire Bool | Tick

update : Action -> Model -> Model
update action ship =
  case action of
    NoOp ->
      ship
    Move x ->
      { ship | position = ship.position + (x * 8) }
    Fire firing ->
      let
        newPowerLevel =
          if firing then ship.powerLevel - 1 else ship.powerLevel
      in
        { ship |
            isFiring = firing,
            powerLevel = newPowerLevel
        }
    Tick ->
      let
        newPowerLevel =
          if ship.powerLevel < 10 then ship.powerLevel + 1 else ship.powerLevel
      in
        { ship | powerLevel = newPowerLevel }


-- VIEW

drawGame : Float -> Float -> Form
drawGame w h =
  rect w h
    |> filled gray

drawShip : Float -> Model -> Form
drawShip gameHeight ship =
  let
    shipColor =
      if ship.isFiring then red else blue
  in
    ngon 3 30
      |> filled shipColor
      |> rotate (degrees 90)
      |> move ((toFloat ship.position), (50 - gameHeight / 2))
      |> alpha ((toFloat ship.powerLevel) / 10)

view : (Int, Int) -> Model -> Element
view (w, h) ship =
  let
    (w', h') = (toFloat w, toFloat h)
  in
  collage w h
    [ drawGame w' h',
      drawShip h' ship,
      toForm(show ship)
    ]


-- SIGNALS

ticker : Signal Action
ticker =
  Signal.map (always Tick) (Time.every Time.second)

fire : Signal Action
fire =
  Signal.map Fire Keyboard.space

direction : Signal Action
direction =
  let
    x = Signal.map .x Keyboard.arrows
    delta = Time.fps 30
  in
    Signal.sampleOn delta (Signal.map Move x)

input : Signal Action
input =
  Signal.mergeMany [direction, fire, ticker]

model : Signal Model
model =
  Signal.foldp update initialShip input


main : Signal Element
main =
  Signal.map2 view Window.dimensions model
