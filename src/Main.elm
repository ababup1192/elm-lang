port module Main exposing
    ( Msg(..)
    , Tree(..)
    , main
    , makeCodeSec
    , makeExportSec
    , makeFuncSec
    , makeString
    , makeTypeSec
    )

import Bitwise
import Browser
import Html exposing (Html, button, div, main_, pre, text, textarea)
import Html.Attributes exposing (class, cols, rows)
import Html.Events exposing (onClick, onInput)
import Parser exposing ((|.), (|=), Parser, int, spaces, succeed, symbol)



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


type alias Function =
    { exported : Bool
    , name : String
    , params : List Int
    , result : List Int
    , local : List Int
    , code : List Int
    }


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


makeFuncType : Function -> Tree
makeFuncType function =
    Branch <|
        [ Leaf 0x60
        , makeVec <| List.map Leaf function.params
        , makeVec <| List.map Leaf function.result
        ]


makeTypeSec : List Function -> Tree
makeTypeSec functions =
    let
        body =
            makeVec <|
                List.map
                    makeFuncType
                    functions
    in
    makeSection 0x01 body


makeFuncSec : List Function -> Tree
makeFuncSec functions =
    let
        body =
            makeVec <|
                List.indexedMap
                    (\index _ ->
                        branch [ index ]
                    )
                    functions
    in
    makeSection 0x03 body


makeExport : String -> Int -> Int -> Tree
makeExport name typeId index =
    Branch [ makeString name, Leaf typeId, Leaf index ]


makeFuncExport : String -> Int -> Tree
makeFuncExport name index =
    makeExport name 0x00 index


makeExportSec : List Function -> Tree
makeExportSec functions =
    let
        body =
            makeVec <|
                List.filterMap
                    (\( index, function ) ->
                        if function.exported then
                            Just <|
                                makeFuncExport function.name index

                        else
                            Nothing
                    )
                <|
                    List.indexedMap
                        Tuple.pair
                    <|
                        functions
    in
    makeSection 0x07 body


makeCode : Function -> Tree
makeCode function =
    let
        locals =
            makeVec <| List.map Leaf function.local

        body =
            Branch [ branch function.code, Leaf 0x0B ]
    in
    Branch
        [ Leaf <| countLeaves locals + countLeaves body
        , locals
        , body
        ]


makeCodeSec : List Function -> Tree
makeCodeSec functions =
    let
        body =
            makeVec <|
                List.map makeCode functions
    in
    makeSection 0x0A body


countLeaves : Tree -> Int
countLeaves tree =
    case tree of
        Leaf _ ->
            1

        Branch trees ->
            List.foldl
                (\t acc ->
                    acc + countLeaves t
                )
                0
                trees


makeSection : Int -> Tree -> Tree
makeSection id body =
    Branch [ Leaf id, Leaf <| countLeaves body, body ]


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


makeInt32 : Int -> List Int
makeInt32 value =
    List.reverse <|
        makeInt32_ value []


makeInt32_ : Int -> List Int -> List Int
makeInt32_ value bytes =
    let
        size =
            ceiling <| logBase 2 <| abs <| toFloat value

        byte =
            Bitwise.and value 127

        nextValue =
            if value < 0 then
                Bitwise.or value <| negate <| Bitwise.shiftLeftBy (size - 7) 1

            else
                Bitwise.shiftRightBy 7 value

        end =
            (nextValue == 0 && Bitwise.and byte 0x40 == 0)
                || (nextValue == -1 && Bitwise.and byte 0x40 == 0x40)
    in
    if end then
        byte :: bytes

    else
        let
            nextByte =
                Bitwise.or byte 128
        in
        makeInt32_ nextValue <| nextByte :: bytes


makeUInt32 : Int -> List Int
makeUInt32 value =
    List.reverse <|
        makeUInt32_ value 0 []


makeUInt32_ : Int -> Int -> List Int -> List Int
makeUInt32_ value pad bytes =
    let
        byte =
            Bitwise.and value 0x7F

        nextValue =
            Bitwise.shiftRightBy 0x07 value
    in
    if nextValue /= 0 || pad > -1 then
        let
            nextByte =
                if nextValue /= 0 || pad > 0 then
                    Bitwise.or byte 0x80

                else
                    byte
        in
        makeUInt32_ nextValue (pad - 1) <| nextByte :: bytes

    else
        byte :: bytes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSource source ->
            let
                functions =
                    [ { exported = True
                      , name = "aa"
                      , params = []
                      , result = [ 0x7F ]
                      , local = []
                      , code = 0x41 :: makeInt32 0xFF
                      }
                    , { exported = True
                      , name = "bb"
                      , params = [ 0x7F ]
                      , result = [ 0x7F ]
                      , local = []
                      , code = [ 0x20, 0x00 ]
                      }
                    ]
            in
            -- case Parser.run addParser source of
            -- Ok biOp ->
            ( { model | source = source }
            , compile <|
                u8tree2u8array <|
                    Branch
                        [ makeMagic
                        , makeVersion
                        , makeTypeSec functions
                        , makeFuncSec functions
                        , makeExportSec functions
                        , makeCodeSec functions
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
