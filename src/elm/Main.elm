module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (Svg, g, polygon, text, text_)
import Svg.Attributes exposing (fill, points, x, y, alignmentBaseline, textAnchor)
import Svg.Events exposing (onMouseOver, onClick)
import Dict
import HexGrid exposing (HexGrid(..))


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
    { title : String
    , grid : HexGrid ()
    , activePoint : HexGrid.Point
    , hoverPoint : HexGrid.Point
    }


init : ( Model, Cmd Msg )
init =
    ( { title = ""
      , grid = HexGrid.empty 5 ()
      , activePoint = ( 0, 0 )
      , hoverPoint = ( -1, -4 )
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | ActivePoint HexGrid.Point
    | HoverPoint HexGrid.Point


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ActivePoint point ->
            ( { model | activePoint = point }, Cmd.none )

        HoverPoint point ->
            ( { model | hoverPoint = point }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ renderHex model ]


renderHex : Model -> Html Msg
renderHex model =
    let
        (HexGrid _ dict) =
            model.grid

        cornersToStr corners =
            corners
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "

        layout =
            HexGrid.mkPointyTop 30 30 (600 / 2) (570 / 2)

        renderPoint ( point, tile ) =
            let
                ( centerX, centerY ) =
                    HexGrid.hexToPixel layout point

                corners =
                    HexGrid.polygonCorners layout point
            in
                g
                    [ onClick (ActivePoint point)
                    , onMouseOver (HoverPoint point)
                    ]
                    [ polygon
                        [ points (cornersToStr <| corners)
                        , fill <|
                            if point == ( 0, 0 ) then
                                "grey"
                            else if model.activePoint == point then
                                "#3498db"
                            else if model.hoverPoint == point then
                                "#f1c40f"
                            else
                                "white"
                        ]
                        []
                    , text_
                        [ x (toString <| centerX)
                        , y (toString <| centerY)
                        , alignmentBaseline "middle"
                        , textAnchor "middle"
                        , Html.Attributes.style [ ( "font-size", "16px" ) ]
                        ]
                        [ text <|
                            if point == model.activePoint then
                                toString model.activePoint
                            else
                                ""
                        ]
                    ]
    in
        Svg.svg
            []
            (List.map renderPoint (Dict.toList dict))
