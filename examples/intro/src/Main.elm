module Main exposing (main)

import Browser
import Browser.Events
import Elm2d
import Html exposing (..)


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
    { canvas : Elm2d.Canvas
    }


init : Flags -> ( Model, Cmd Msg )
init { window } =
    ( { canvas = canvas { window = window, x = 120, y = 50 }
      }
    , Cmd.none
    )


canvas :
    { window : { width : Int, height : Int }
    , x : Int
    , y : Int
    }
    -> Elm2d.Canvas
canvas options =
    Elm2d.canvas
        { size = options.window
        , background = ( 0, 175, 125 )
        }
        -- face
        |> Elm2d.draw
            (Elm2d.rectangle
                { color = ( 220, 170, 120 )
                , position = ( options.x, options.y )
                , size = ( 48, 48 )
                }
            )
        -- left eye
        |> Elm2d.draw
            (Elm2d.rectangle
                { color = ( 255, 255, 255 )
                , position = ( options.x + 8, options.y + 16 )
                , size = ( 12, 16 )
                }
            )
        |> Elm2d.draw
            (Elm2d.rectangle
                { color = ( 0, 0, 0 )
                , position = ( options.x + 14, options.y + 16 )
                , size = ( 6, 16 )
                }
            )
        -- right eye
        |> Elm2d.draw
            (Elm2d.rectangle
                { color = ( 255, 255, 255 )
                , position = ( options.x + 28, options.y + 16 )
                , size = ( 12, 16 )
                }
            )
        |> Elm2d.draw
            (Elm2d.rectangle
                { color = ( 0, 0, 0 )
                , position = ( options.x + 28, options.y + 16 )
                , size = ( 6, 16 )
                }
            )


type Msg
    = ResizedWindow Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizedWindow width height ->
            ( { model
                | canvas =
                    Elm2d.resize
                        { width = width
                        , height = height
                        }
                        model.canvas
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize ResizedWindow


view : Model -> Browser.Document Msg
view model =
    { title = "Elm2d Examples | Intro"
    , body =
        [ Elm2d.toHtml model.canvas
        ]
    }
