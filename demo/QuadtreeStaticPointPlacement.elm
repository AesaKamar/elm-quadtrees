module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HtmlA
import Window exposing (..)
import Quadtree exposing (..)
import Point exposing (..)
import Task exposing (perform)
import Bound exposing (..)
import Svg
import Svg.Attributes as SvgA
import List exposing ((::))
import Pointer exposing (onDown)
import Random as Rnd


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { quadTree : QuadTree Point
    , windowSize : Bound
    , randomSeed : Rnd.Seed
    }


initialPoints =
    [-- { x = 40, y = 40 }
     -- , { x = 600, y = 200 }
     -- , { x = 1100, y = 100 }
     -- , { x = 1000, y = 100 }
     -- , { x = 500, y = 1000 }
     -- , { x = 650, y = 600 }
    ]


initialModel : Model
initialModel =
    { quadTree = emptyQuadTree
    , windowSize =
        { topLeftmost = { x = 0, y = 0 }
        , botRightmost = { x = 100, y = 100 }
        }
    , randomSeed = Rnd.initialSeed 12345
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Task.perform WindowResize Window.size
    )



-- UPDATE


type Msg
    = WindowResize Window.Size
    | InsertPoint (List Point)
    | PointerDown ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize windowSizing ->
            let
                bound =
                    { topLeftmost = { x = 0, y = 0 }
                    , botRightmost =
                        { x = windowSizing.width |> toFloat
                        , y = windowSizing.height |> toFloat
                        }
                    }
            in
                ( { quadTree = External Nothing bound
                  , windowSize = bound
                  , randomSeed = model.randomSeed
                  }
                , sendMessage (InsertPoint initialPoints)
                )

        InsertPoint pts ->
            case pts of
                h :: t ->
                    ( { quadTree = insert model.quadTree h
                      , windowSize = model.windowSize
                      , randomSeed = model.randomSeed
                      }
                    , sendMessage (InsertPoint t)
                    )

                [] ->
                    ( { quadTree = model.quadTree
                      , windowSize = model.windowSize
                      , randomSeed = model.randomSeed
                      }
                    , Cmd.none
                    )

        PointerDown ( x, y ) ->
            let
                rndx =
                    Rnd.step (Rnd.float -0.001 0.001) model.randomSeed

                rndy =
                    Rnd.step (Rnd.float -0.001 0.001) model.randomSeed
            in
                ( model
                , sendMessage (InsertPoint [ { x = x, y = y } ])
                )


sendMessage : a -> Cmd a
sendMessage x =
    Task.succeed x
        |> Task.perform identity



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowResize



-- VIEW


viewQuadTree : QuadTree Point -> List (Svg.Svg Msg)
viewQuadTree qt =
    case qt of
        Internal ( aQuadTree, aQuadTree2, aQuadTree3, aQuadTree4 ) bound ->
            (viewQuadTree aQuadTree)
                ++ (viewQuadTree aQuadTree2)
                ++ (viewQuadTree aQuadTree3)
                ++ (viewQuadTree aQuadTree4)

        External aMaybe bound ->
            case aMaybe of
                Just pt ->
                    [ drawBound bound
                    , drawPoint pt
                    ]

                Nothing ->
                    [ drawBound bound ]


drawPoint pt =
    Svg.circle
        [ SvgA.cx (pt.x |> toString)
        , SvgA.cy (pt.y |> toString)
        , SvgA.r ("3")
        , SvgA.fill "red"
        ]
        []


drawBound bound =
    Svg.rect
        [ SvgA.x (bound.topLeftmost.x |> toString)
        , SvgA.y (bound.topLeftmost.y |> toString)
        , SvgA.width ((bound.botRightmost.x - bound.topLeftmost.x) |> toString)
        , SvgA.height ((bound.botRightmost.y - bound.topLeftmost.y) |> toString)
        , SvgA.style "outline-style:solid; outline-offset:-1px; outline-width:2px; outline-color:black"
        , SvgA.fill "none"
        ]
        []


view : Model -> Html Msg
view model =
    div [ onDown (relativePos >> PointerDown) ]
        [ Svg.svg
            [ SvgA.height (model.windowSize.botRightmost.y |> toString)
            , SvgA.width (model.windowSize.botRightmost.x |> toString)
            ]
            (viewQuadTree model.quadTree)
        ]


relativePos : Pointer.Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos
