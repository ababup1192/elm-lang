module MainTest exposing (..)

import Expect
import Fuzz exposing (string)
import Main exposing (..)
import Test exposing (..)


makeTypeSecTest : Test
makeTypeSecTest =
    describe "makeTypeSec test"
        [ test "makeTypeSec は以下のように展開される" <|
            \_ ->
                makeTypeSec
                    |> Expect.equal
                        (Branch
                            [ Leaf 0x01
                            , Leaf 0x04
                            , Branch
                                [ Leaf 0x01
                                , Branch
                                    [ Branch [ Leaf 0x60, Leaf 0x00, Leaf 0x00 ]
                                    ]
                                ]
                            ]
                        )
        ]


makeFuncSecTest : Test
makeFuncSecTest =
    describe "makeFuncSec test"
        [ test "makeFuncSec は以下のように展開される" <|
            \_ ->
                makeFuncSec
                    |> Expect.equal
                        (Branch
                            [ Leaf 0x03
                            , Leaf 0x02
                            , Branch
                                [ Leaf 0x01
                                , Branch
                                    [ Branch [ Leaf 0x00 ]
                                    ]
                                ]
                            ]
                        )
        ]


makeExportSecTest : Test
makeExportSecTest =
    describe "makeExportSec test"
        [ test "makeExportSec は以下のように展開される" <|
            \_ ->
                makeExportSec
                    |> Expect.equal
                        (Branch
                            [ Leaf 0x07
                            , Leaf 0x05
                            , Branch
                                [ Leaf 0x01
                                , Branch
                                    [ Branch
                                        [ makeString "a"
                                        , Leaf 0x00
                                        , Leaf 0x00
                                        ]
                                    ]
                                ]
                            ]
                        )
        ]


makeCodeSecTest : Test
makeCodeSecTest =
    describe "makeCodeSec test"
        [ test "makeCodeSec は以下のように展開される" <|
            \_ ->
                makeCodeSec
                    |> Expect.equal
                        (Branch
                            [ Leaf 0x0A
                            , Leaf 0x04
                            , Branch
                                [ Leaf 0x01
                                , Branch
                                    [ Branch
                                        [ Leaf 0x02
                                        , Leaf 0x00
                                        , Leaf 0x0B
                                        ]
                                    ]
                                ]
                            ]
                        )
        ]
