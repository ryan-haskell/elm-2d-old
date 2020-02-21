module Main exposing (main)

import Browser
import Browser.Events
import Elm2D
import Html exposing (..)
import Html.Attributes as Attr
import Json.Decode as D exposing (Decoder)
import Set exposing (Set)


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


type alias Flags =
    { window :
        { width : Int
        , height : Int
        }
    }


type alias Model =
    { window :
        { width : Int
        , height : Int
        }
    , position : ( Float, Float )
    , keys : Set ( Float, Float )
    }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init { window } =
    ( { window = window
      , position =
            ( toFloat window.width / 2 - 24
            , toFloat window.height / 2 - 24
            )
      , keys = Set.empty
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ResizedWindow Int Int
    | PressedKey Key
    | ReleasedKey Key
    | GotRenderFrame Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizedWindow width height ->
            ( { model
                | window =
                    { width = width
                    , height = height
                    }
              }
            , Cmd.none
            )

        PressedKey key ->
            ( { model | keys = Set.insert (toTuple key) model.keys }
            , Cmd.none
            )

        ReleasedKey key ->
            ( { model | keys = Set.remove (toTuple key) model.keys }
            , Cmd.none
            )

        GotRenderFrame timeElapsed ->
            ( { model | position = updatePosition timeElapsed model.position model.keys }
            , Cmd.none
            )


updatePosition :
    Float
    -> ( Float, Float )
    -> Set ( Float, Float )
    -> ( Float, Float )
updatePosition time =
    let
        speed : Float
        speed =
            0.35
    in
    Set.foldl
        (\( dx, dy ) ( x, y ) ->
            ( dx * time * speed + x
            , dy * time * speed + y
            )
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize ResizedWindow
        , Browser.Events.onKeyDown (keyDecoder PressedKey)
        , Browser.Events.onKeyUp (keyDecoder ReleasedKey)
        , Browser.Events.onAnimationFrameDelta GotRenderFrame
        ]



-- KEYBOARD CONTROLS


type Key
    = Up
    | Left
    | Right
    | Down


toTuple : Key -> ( Float, Float )
toTuple key =
    case key of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


keyDecoder : (Key -> Msg) -> Decoder Msg
keyDecoder toMsg =
    D.field "code" D.string
        |> D.andThen
            (\str ->
                case str of
                    "KeyW" ->
                        D.succeed (toMsg Up)

                    "KeyA" ->
                        D.succeed (toMsg Left)

                    "KeyS" ->
                        D.succeed (toMsg Down)

                    "KeyD" ->
                        D.succeed (toMsg Right)

                    _ ->
                        D.fail "Invalid key"
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm2D Examples | Intro"
    , body =
        [ Elm2D.toHtml <|
            canvas
                { window = model.window
                , position = Tuple.mapBoth round round model.position
                }
        , div
            [ Attr.style "position" "fixed"
            , Attr.style "z-index" "1"
            , Attr.style "right" "2rem"
            , Attr.style "top" "2rem"
            , Attr.style "background" "white"
            , Attr.style "padding" "1rem 2rem"
            , Attr.style "box-shadow" "0 4px 8px rgba(0,0,0,0.25)"
            , Attr.style "border-radius" "2px"
            , Attr.style "font-family" "sans-serif"
            ]
            [ text "Use WASD to move!"
            ]
        ]
    }



-- ELM 2D


canvas :
    { model
        | window : { width : Int, height : Int }
        , position : ( Int, Int )
    }
    -> Elm2D.Canvas
canvas options =
    let
        ( x, y ) =
            options.position
    in
    Elm2D.canvas
        { size = options.window
        , background = ( 0, 175, 125 )
        }
        -- face
        |> Elm2D.draw
            (Elm2D.rectangle
                { color = ( 220, 170, 120 )
                , position = ( x, y )
                , size = ( 48, 48 )
                }
            )
        -- left eye
        |> Elm2D.draw
            (Elm2D.rectangle
                { color = ( 255, 255, 255 )
                , position = ( x + 8, y + 16 )
                , size = ( 12, 16 )
                }
            )
        |> Elm2D.draw
            (Elm2D.rectangle
                { color = ( 0, 0, 0 )
                , position = ( x + 14, y + 16 )
                , size = ( 6, 16 )
                }
            )
        -- right eye
        |> Elm2D.draw
            (Elm2D.rectangle
                { color = ( 255, 255, 255 )
                , position = ( x + 28, y + 16 )
                , size = ( 12, 16 )
                }
            )
        |> Elm2D.draw
            (Elm2D.rectangle
                { color = ( 0, 0, 0 )
                , position = ( x + 28, y + 16 )
                , size = ( 6, 16 )
                }
            )
