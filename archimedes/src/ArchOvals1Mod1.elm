module ArchOvals1Mod1 where

import Array
import StartApp
import Task exposing (Task)
import Signal exposing (Signal, Address)
import Effects exposing (Effects, Never)
import Html exposing (Html)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

--
-- StartApp boilerplate
--
app =
    StartApp.start {
      init = init,
      view = view,
      update = update,
      inputs = []
    }

main : Signal Html
main =
    app.html

port tasks : Signal (Task Never ())
port tasks =
    app.tasks

-------------------------------------------------------------------------------------
--- please see ArchOvals1.elm for notes on source code and project configuration ----
-------------------------------------------------------------------------------------

-- type declarations
--
type alias Struct =
  {
    cx : Float,
    cy : Float,
    rx : Int,
    ry : Int,
    c  : String
  }

type alias Model =
  {
    name : String,
    structures : List Struct
  }

type Action = NoOp

-- functions
--
initialModel : Model
initialModel =
  {
    name = "ArchOvals1Mod1",
    structures = List.reverse (generate 0 721 [])
  }

colors : Array.Array String
colors = Array.fromList ["#FF0000", "#0000FF"]

-- TODO #3: convert imperative logic to functional logic
generate : Int -> Int -> List Struct -> List Struct
generate angle maxAngle structures =
  if angle >= maxAngle
  then
    structures
  else
    let
      delta      = 1
      angle'     = angle + delta
      constant   = 0.25
      radius     = constant * (toFloat angle')
      offsetX    = radius * cos ((toFloat angle') * (pi/180.0))
      offsetY    = radius * sin ((toFloat angle') * (pi/180.0))
      basePointX = 250.0
      basePointY = 250.0
      currentX   = basePointX + offsetX
      currentY   = basePointY - offsetY
      majorAxis  = 40
      minorAxis  = 60
      modulusX   = angle' % majorAxis
      modulusY   = angle' % minorAxis
      color      = Maybe.withDefault "#FF00FF" (Array.get (angle' % 2) colors) 
    in
      generate
        angle'
        maxAngle
        ({ cx = currentX, cy = currentY, rx = modulusX, ry = modulusY, c = color } :: structures)


init : (Model, Effects Action)
init = ( initialModel, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
          NoOp -> (model, Effects.none)


view : Address Action -> Model -> Html
view address model =
    svg
      [ width "600", height "500" ]
      (List.map transformSingle model.structures)


-- TODO #1: create transformation logic: single structure to single Svg element
transformSingle : Struct -> Svg
transformSingle struct =
  ellipse
    [ cx (toString struct.cx), cy (toString struct.cy), rx (toString struct.rx), ry (toString struct.ry), fill (struct.c) ]
    []

-- TODO #2: create transformation logic: list of structures to list of Svg elements
-- function map does this (observe the call to List.map within function view)
