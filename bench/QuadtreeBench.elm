module QuadtreeBench exposing (..)

import Dict
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Quadtree exposing (..)
import Point exposing (..)
import Bound exposing (..)


main : BenchmarkProgram
main =
    program suite


outerBound : Bound
outerBound =
    { topLeftmost = { x = 0, y = 0 }
    , botRightmost = { x = 100, y = 100 }
    }


emptyQuadTree : QuadTree Point
emptyQuadTree =
    External Nothing outerBound


pointA : Point
pointA =
    { x = 1, y = 1 }


pointB : Point
pointB =
    { x = 2, y = 2 }


pointC : Point
pointC =
    { x = 1.01, y = 1.01 }


suite : Benchmark
suite =
    describe "Benchmarks"
        [ (benchmark "Baseline" <| (\_ -> 1 + 1))
        , (benchmark "Quadtree.insert shallow" (\_ -> insert emptyQuadTree pointA))
        , (benchmark "Quadtree.insert deep" (\_ -> insert (insert emptyQuadTree pointA) pointB))
        , (benchmark "Quadtree.insert ultra deep" (\_ -> insert (insert emptyQuadTree pointA) pointC))
        ]
