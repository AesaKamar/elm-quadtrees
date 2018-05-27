module Dynamics exposing (..)

import Point exposing (..)


type alias TimeDelta =
    Float


type alias Displacement =
    { dx : Float, dy : Float }


type alias Velocity =
    { vx : Float, vy : Float }


type alias Acceleration =
    { ax : Float, ay : Float }


type alias Mass =
    Float


type alias Force =
    { fx : Float, fy : Float }


type alias DynamicPoint =
    { p : Point, m : Mass, v : Velocity, a : Acceleration }


accelerate : TimeDelta -> Acceleration -> Velocity
accelerate t a =
    { vx = a.ax * t, vy = a.ay * t }


displace : TimeDelta -> Velocity -> Displacement
displace t v =
    { dx = v.vx * t, dy = v.vy * t }


push : Mass -> Force -> Acceleration
push m f =
    { ax = f.fx / m, ay = f.fy / m }
