port module Main exposing (Msg(..), main, update)

import Browser
import Html exposing (Html, button, div, main_, pre, text, textarea)
import Html.Attributes exposing (class, cols, rows)
import Html.Events exposing (onClick, onInput)
import Parser exposing ((|.), (|=), Parser, int, spaces, succeed, symbol)
import Test.Html.Query exposing (count)



-- elm -> js


port compile : List Int -> Cmd msg



-- js -> elm


port receiveWasm : (String -> msg) -> Sub msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { source : String, wasm : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { source = "", wasm = "" }, Cmd.none )



-- UPDATE


type Msg
    = UpdateSource String
    | ReceiveWasm String


compile2Wat : BiOp -> String
compile2Wat biOp =
    "(module"
        ++ "(func (export \"add\") (result i32)\n"
        ++ (case biOp of
                Add l r ->
                    "i32.const "
                        ++ String.fromInt l
                        ++ "\n"
                        ++ "i32.const "
                        ++ String.fromInt r
                        ++ "\n"
           )
        ++ "i32.add))\n"


type Tree
    = Leaf Int
    | Branch (List Tree)


makeVec : List Tree -> Tree
makeVec trees =
    Branch [ Leaf <| List.length trees, Branch trees ]


makeString : String -> Tree
makeString s =
    Branch [ Leaf <| String.length s, branch <| List.map Char.toCode <| String.toList s ]


branch : List Int -> Tree
branch uint8array =
    Branch <| List.map Leaf uint8array


makeMagic : Tree
makeMagic =
    branch [ 0x00, 0x61, 0x73, 0x6D ]


makeVersion : Tree
makeVersion =
    branch [ 0x01, 0x00, 0x00, 0x00 ]


makeTypeSec : Tree
makeTypeSec =
    Branch
        [ -- section id: type section
          Leaf 0x01

        -- section size
        , Leaf 0x04
        , makeVec
            [ branch
                [ 0x60
                , 0x00
                , 0x00
                ]
            ]
        ]


makeFuncSec : Tree
makeFuncSec =
    Branch
        [ Leaf 0x03
        , Leaf 0x02
        , makeVec [ branch [ 0x00 ] ]
        ]


makeExportSec : Tree
makeExportSec =
    Branch
        [ Leaf 0x07
        , Leaf 0x05
        , makeVec
            [ Branch
                [ makeString "a"
                , Leaf 0x00
                , Leaf 0x00
                ]
            ]
        ]


makeCodeSec : Tree
makeCodeSec =
    Branch
        [ Leaf 0x0A
        , Leaf 0x04
        , makeVec
            [ branch
                [ 0x02
                , 0x00
                , 0x0B
                ]
            ]
        ]


u8tree2u8array : Tree -> List Int
u8tree2u8array tree =
    let
        emit node =
            case node of
                Leaf x ->
                    [ x ]

                Branch [] ->
                    []

                Branch (t :: ts) ->
                    emit t ++ List.concatMap emit ts
    in
    emit tree


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSource source ->
            -- case Parser.run addParser source of
            -- Ok biOp ->
            ( { model | source = source }
            , compile <|
                u8tree2u8array <|
                    Branch
                        [ makeMagic
                        , makeVersion
                        , makeTypeSec
                        , makeFuncSec
                        , makeExportSec
                        , makeCodeSec
                        ]
            )

        -- Err _ ->
        -- ( { model | wasm = "Parse error." }, Cmd.none )
        ReceiveWasm wasm ->
            ( { model | wasm = wasm }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ class "ly_cont" ]
        [ textarea [ onInput UpdateSource, rows 20, cols 80 ] [ text model.source ]
        , div [] [ pre [] [ text model.wasm ] ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveWasm ReceiveWasm


type BiOp
    = Add Int Int


addParser : Parser BiOp
addParser =
    succeed Add
        |. spaces
        |= int
        |. spaces
        |. symbol "+"
        |. spaces
        |= int
        |. spaces
