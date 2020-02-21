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
    ( { canvas =
            Elm2d.canvas
                { size = window
                , background = ( 0, 175, 125 )
                }
                |> Elm2d.draw
                    (Elm2d.rectangle
                        { color = ( 0, 75, 150 )
                        , position = ( 100, 100 )
                        , size = ( 48, 48 )
                        }
                    )
      }
    , Cmd.none
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
