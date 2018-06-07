module Dynamics exposing (..)

import Pointlike exposing (..)


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


type alias MassPoint =
    { p : Pointlike, m : Mass }


type alias DynamicPoint mp =
    { mp | mp : MassPoint, v : Velocity, a : Acceleration }


accelerate : TimeDelta -> Acceleration -> Velocity
accelerate t a =
    { vx = a.ax * t, vy = a.ay * t }


displace : TimeDelta -> Velocity -> Displacement
displace t v =
    { dx = v.vx * t, dy = v.vy * t }


applyForce : Mass -> Force -> Acceleration
applyForce m f =
    { ax = f.fx / m, ay = f.fy / m }
