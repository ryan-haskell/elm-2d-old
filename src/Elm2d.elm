module Elm2d exposing
    ( Canvas, canvas, resize, background
    , toHtml
    , Shape, draw, rectangle
    )

{-|

@docs Canvas, canvas, resize, background

@docs toHtml

@docs Shape, draw, rectangle

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import WebGL


type Canvas
    = Canvas
        { size : Size
        , background : Color
        , shapes : List Shape
        }


canvas :
    { size : { width : Int, height : Int }
    , background : ( Int, Int, Int )
    }
    -> Canvas
canvas options =
    Canvas
        { size = toSize options.size
        , background = toColor options.background
        , shapes = []
        }


resize : { width : Int, height : Int } -> Canvas -> Canvas
resize size (Canvas c) =
    Canvas { c | size = toSize size }


background : ( Int, Int, Int ) -> Canvas -> Canvas
background color (Canvas c) =
    Canvas { c | background = toColor color }



-- SHAPES


type Shape
    = Rectangle RectangleOptions


type alias RectangleOptions =
    { color : Color
    , position : Position
    , size : Size
    }


draw : Shape -> Canvas -> Canvas
draw shape_ (Canvas c) =
    Canvas { c | shapes = shape_ :: c.shapes }


toEntity : Size -> Shape -> WebGL.Entity
toEntity (Size w h) shape =
    let
        viewRectangle : RectangleOptions -> WebGL.Entity
        viewRectangle options =
            let
                uniforms : Color -> { color : Vec3, canvas : Vec2 }
                uniforms (Color r g b) =
                    { color = Vec3.vec3 r g b
                    , canvas = Vec2.vec2 (2 / w) (2 / h)
                    }

                mesh : WebGL.Mesh { position : Vec2 }
                mesh =
                    WebGL.triangles (rectangleTriangles options.position options.size)
            in
            WebGL.entity vertexShader solid mesh (uniforms options.color)
    in
    case shape of
        Rectangle options ->
            viewRectangle options


rectangleTriangles :
    Position
    -> Size
    -> List ( { position : Vec2 }, { position : Vec2 }, { position : Vec2 } )
rectangleTriangles (Position x y) (Size w h) =
    [ ( Vec2.vec2 x y
      , Vec2.vec2 (x + w) y
      , Vec2.vec2 (x + w) (y + h)
      )
    , ( Vec2.vec2 (x + w) (y + h)
      , Vec2.vec2 x (y + h)
      , Vec2.vec2 x y
      )
    ]
        |> List.map
            (\( a, b, c ) ->
                ( { position = a }
                , { position = b }
                , { position = c }
                )
            )


rectangle :
    { color : ( Int, Int, Int )
    , position : ( Int, Int )
    , size : ( Int, Int )
    }
    -> Shape
rectangle options =
    let
        ( w, h ) =
            Tuple.mapBoth toFloat toFloat options.size

        ( x, y ) =
            Tuple.mapBoth toFloat toFloat options.position
    in
    Rectangle
        { color = toColor options.color
        , position = Position x y
        , size = Size w h
        }


type Size
    = Size Float Float


toSize : { width : Int, height : Int } -> Size
toSize { width, height } =
    Size (toFloat width) (toFloat height)


sizeWidth : Size -> Int
sizeWidth (Size x _) =
    floor x


sizeHeight : Size -> Int
sizeHeight (Size _ y) =
    floor y


type Position
    = Position Float Float


type Color
    = Color Float Float Float


toColor : ( Int, Int, Int ) -> Color
toColor ( r, g, b ) =
    Color
        (toFloat r / 255)
        (toFloat g / 255)
        (toFloat b / 255)



-- RENDERING


toHtml : Canvas -> Html msg
toHtml (Canvas c) =
    WebGL.toHtmlWith
        [ c.background |> (\(Color r g b) -> WebGL.clearColor r g b 1)
        ]
        [ Attr.width (sizeWidth c.size)
        , Attr.height (sizeHeight c.size)
        , c.background
            |> rgbToString
            |> Attr.style "background-color"
        ]
        (c.shapes |> List.reverse |> List.map (toEntity c.size))



-- VERTEX SHADERS
-- 2 / canvas_width
-- 2 / canvas_height


vertexShader : WebGL.Shader { position : Vec2 } { u | canvas : Vec2 } {}
vertexShader =
    [glsl|
        attribute vec2 position;
        uniform vec2 canvas;
        void main () {
            gl_Position =
                vec4(canvas, 1, 1)
                * ( vec4(1, -1, 1, 1) * vec4(position, 0, 1.0))
                    + vec4(-1, 1, 0, 0);
        }
    |]



-- FRAGMENT SHADERS


{-| A very simple shader, coloring the whole area in a single color
-}
solid : WebGL.Shader {} { u | color : Vec3 } {}
solid =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        void main() {
            gl_FragColor = vec4(color, 1);
        }
    |]



-- INTERNALS


rgbToString : Color -> String
rgbToString (Color r g b) =
    String.concat
        [ "rgb("
        , String.fromFloat r
        , ", "
        , String.fromFloat g
        , ", "
        , String.fromFloat b
        , ")"
        ]
