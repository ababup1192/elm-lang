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
                    [ { exported = True
                      , name = "function"
                      , params = []
                      , result = []
                      , local = []
                      , code = []
                      }
                    ]
                    |> Expect.equal
                        (Branch
                            [ Leaf 0x01
                            , Leaf 0x04
                            , Branch
                                [ Leaf 0x01
                                , Branch
                                    [ Branch
                                        [ Leaf 0x60
                                        , Branch
                                            [ Leaf 0x00
                                            , Branch []
                                            ]
                                        , Branch
                                            [ Leaf 0x00
                                            , Branch []
                                            ]
                                        ]
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
                    [ { exported = True
                      , name = "a"
                      , params = []
                      , result = []
                      , local = []
                      , code = []
                      }
                    ]
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
                    [ { exported = True
                      , name = "a"
                      , params = []
                      , result = []
                      , local = []
                      , code = []
                      }
                    ]
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
                    [ { exported = True
                      , name = "a"
                      , params = []
                      , result = []
                      , local = []
                      , code = []
                      }
                    ]
                    |> Expect.equal
                        (Branch
                            [ Leaf 0x0A
                            , Leaf 0x04
                            , Branch
                                [ Leaf 0x01
                                , Branch
                                    [ Branch
                                        [ Leaf 0x02
                                        , Branch
                                            [ Leaf 0
                                            , Branch []
                                            ]
                                        , Branch
                                            [ Branch []
                                            , Leaf 0x0B
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        )
        ]
