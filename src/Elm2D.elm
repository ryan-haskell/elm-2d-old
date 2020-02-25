module Elm2D exposing
    ( Canvas, canvas, resize, background
    , toHtml
    , Shape, rectangle, sprite
    , Texture, texture
    )

{-|

@docs Canvas, canvas, resize, background

@docs toHtml

@docs Shape, rectangle, sprite

@docs Texture, texture

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Task
import WebGL
import WebGL.Texture as Texture



-- CANVAS


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
    -> List Shape
    -> Canvas
canvas options shapes =
    Canvas
        { size = toSize options.size
        , background = toColor options.background
        , shapes = shapes
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
    | Sprite SpriteOptions


type alias RectangleOptions =
    { color : Color
    , position : Position
    , size : Size
    }


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


type alias SpriteOptions =
    { texture : Texture
    , selection :
        { position : Position
        , size : Size
        }
    , position : Position
    , size : Size
    }


sprite :
    { texture : Texture
    , selection : { position : ( Int, Int ), size : ( Int, Int ) }
    , position : ( Int, Int )
    , size : ( Int, Int )
    }
    -> Shape
sprite options =
    let
        ( w, h ) =
            Tuple.mapBoth toFloat toFloat options.size

        ( x, y ) =
            Tuple.mapBoth toFloat toFloat options.position

        ( sw, sh ) =
            Tuple.mapBoth toFloat toFloat options.selection.size

        ( sx, sy ) =
            Tuple.mapBoth toFloat toFloat options.selection.position
    in
    Sprite
        { texture = options.texture
        , selection =
            { position = Position sx sy
            , size = Size sw sh
            }
        , position = Position x y
        , size = Size w h
        }



-- TEXTURES


type alias Texture =
    Texture.Texture


texture :
    { url : String
    , onLoad : Maybe Texture -> msg
    }
    -> Cmd msg
texture options =
    Texture.loadWith
        (Texture.defaultOptions |> (\opts -> { opts | magnify = Texture.nearest }))
        options.url
        |> Task.attempt (Result.toMaybe >> options.onLoad)



-- RENDERING


toHtml : Canvas -> Html msg
toHtml (Canvas c) =
    WebGL.toHtmlWith
        [ c.background |> (\(Color r g b) -> WebGL.clearColor r g b 1)
        ]
        [ Attr.width (sizeWidth c.size)
        , Attr.height (sizeHeight c.size)
        ]
        (List.map (toEntity c.size) c.shapes)



-- RENDERING ENTITIES


toEntity : Size -> Shape -> WebGL.Entity
toEntity size shape =
    case shape of
        Rectangle options ->
            viewRectangle size options

        Sprite options ->
            viewSprite size options



-- RENDERING RECTANGLES


type alias RectangleVertex =
    { position : Vec2
    }


type alias RectangleUniforms =
    { color : Vec3
    , canvas : Vec2
    }


type alias RectangleVaryings =
    {}


viewRectangle : Size -> RectangleOptions -> WebGL.Entity
viewRectangle (Size w h) options =
    let
        uniforms : Color -> RectangleUniforms
        uniforms (Color r g b) =
            { color = Vec3.vec3 r g b
            , canvas = Vec2.vec2 (2 / w) (2 / h)
            }

        mesh : WebGL.Mesh RectangleVertex
        mesh =
            WebGL.triangles
                (rectangleTriangles
                    options.position
                    options.size
                )
    in
    WebGL.entity rectangleVertexShader rectangleFragmentShader mesh (uniforms options.color)


rectangleTriangles :
    Position
    -> Size
    -> List ( RectangleVertex, RectangleVertex, RectangleVertex )
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


rectangleVertexShader : WebGL.Shader RectangleVertex RectangleUniforms RectangleVaryings
rectangleVertexShader =
    [glsl|
        attribute vec2 position;
        uniform vec2 canvas;
        void main () {
            gl_Position =
                vec4(canvas, 1, 1) * vec4(1, -1, 1, 1) * vec4(position, 0, 1.0) + vec4(-1, 1, 0, 0);
        }
    |]


rectangleFragmentShader : WebGL.Shader {} RectangleUniforms RectangleVaryings
rectangleFragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        void main() {
            gl_FragColor = vec4(color, 1);
        }
    |]



-- RENDERING SPRITES


type alias SpriteVertex =
    { position : Vec2
    , coord : Vec2
    }


type alias SpriteUniforms =
    { texture : Texture
    , canvas : Vec2
    }


type alias SpriteVaryings =
    { vcoord : Vec2
    }


viewSprite : Size -> SpriteOptions -> WebGL.Entity
viewSprite (Size canvasW canvasH) options =
    let
        uniforms : Texture -> SpriteUniforms
        uniforms texture_ =
            { texture = texture_
            , canvas = Vec2.vec2 (2 / canvasW) (2 / canvasH)
            }

        mesh : Size -> Position -> WebGL.Mesh SpriteVertex
        mesh (Size w h) (Position x y) =
            let
                ( Size selectionWidth selectionHeight, Position selectionX selectionY ) =
                    ( options.selection.size
                    , options.selection.position
                    )

                ( textureWidth, textureHeight ) =
                    Tuple.mapBoth toFloat toFloat (Texture.size options.texture)

                ( sx, sy ) =
                    ( selectionX / textureWidth
                    , selectionY / textureHeight
                    )

                ( sw, sh ) =
                    ( selectionWidth / textureWidth
                    , selectionHeight / textureHeight
                    )
            in
            WebGL.indexedTriangles
                [ { position = Vec2.vec2 x y
                  , coord = Vec2.vec2 sx sy
                  }
                , { position = Vec2.vec2 (w + x) y
                  , coord = Vec2.vec2 (sx + sw) sy
                  }
                , { position = Vec2.vec2 (w + x) (h + y)
                  , coord = Vec2.vec2 (sx + sw) (sy + sh)
                  }
                , { position = Vec2.vec2 x (h + y)
                  , coord = Vec2.vec2 sx (sy + sh)
                  }
                ]
                [ ( 0, 1, 2 ), ( 2, 3, 0 ) ]
    in
    WebGL.entity spriteVertexShader spriteFragmentShader (mesh options.size options.position) (uniforms options.texture)


spriteVertexShader : WebGL.Shader SpriteVertex SpriteUniforms SpriteVaryings
spriteVertexShader =
    [glsl|
        attribute vec2 position;
        attribute vec2 coord;
        uniform vec2 canvas;
        varying vec2 vcoord;
        void main () {
            gl_Position = vec4(canvas, 1, 1) * vec4(1, -1, 1, 1) * vec4(position, 0, 1.0) + vec4(-1, 1, 0, 0);
            vcoord = coord.xy * vec2(1, -1);
        }
    |]


spriteFragmentShader : WebGL.Shader {} SpriteUniforms SpriteVaryings
spriteFragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        void main () {
            gl_FragColor = texture2D(texture, vcoord);
            if (gl_FragColor.a == 0.0) {
              discard;
            }
        }
    |]



-- INTERNALS


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
