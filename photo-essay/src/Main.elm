module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Task
import Html exposing (..)
import Html.Attributes exposing (src, class, id)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, map2, map3, map4, map5, field, string, list)



---- MODEL ----


type Model
    = Running BlogState
    | Loading
    | Error

type alias BlogState =
    {
    segmentsRevealed: Int,
    content: Content
    }

type alias Content =
    {
    title: String,
    subtitle: String,
    coverImage: String,
    author: String,
    segments : List Segment}

type alias Segment =
    {
    text: List String,
    images: List String,
    textPosition: String
    }

type TextPosition = Right | Left | Above | Below

init : ( Model, Cmd Msg )
init =
    ( Loading, readTemplate )



---- UPDATE ----


type Msg
    = NoOp
    | NewContent (Result Http.Error Content)
    | Reveal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        NewContent result ->
            case result of
                Ok content ->
                    ( Running { 
                        segmentsRevealed = 0,
                        content = {
                            title = content.title
                            , subtitle = content.subtitle
                            , coverImage = content.coverImage
                            , author = content.author
                            , segments = content.segments
                            }
                        }, Cmd.none)
                Err _ ->
                    (Error, Cmd.none)
        Reveal ->
            case model of
                Running blogState ->
                    (Running { blogState | segmentsRevealed = blogState.segmentsRevealed + 1 }, 
                             jumpToBottom "container")
                _ -> (model, Cmd.none)


readTemplate : Cmd Msg
readTemplate =
    Http.get {
        url = "http://localhost:5019/content"
        , expect = Http.expectJson NewContent contentDecoder
    }

contentDecoder : Decoder Content
contentDecoder =
    map5 Content
        (field "title" string)
        (field "subtitle" string)
        (field "coverImage" string)
        (field "author" string)
        (field "segments" (list segmentDecoder))


segmentDecoder : Decoder Segment
segmentDecoder =
    map3 Segment
        (field "text" (list string))
        (field "images" (list string))
        (field "textPosition" string)

jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)

---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [] [ text "Loading" ]
        Error ->
            div [] [ text "Error" ]
        Running blogState ->
            viewBlog blogState

viewBlog : BlogState -> Html Msg
viewBlog blogState =
    let
        titleHtml =
            div [id "Title"][ h1 [] [text blogState.content.title]
                                   , coverImageHTML blogState.content.coverImage
                                   , h2 [] [text blogState.content.subtitle]
                                   , h3 [] [text blogState.content.author]
                                   ]

        revealedSegmentsHtml = 
            List.take blogState.segmentsRevealed blogState.content.segments
            |> List.map (\segment ->
                segmentHtml segment
                )
            |> div []

        revealButtonHtml =
            if blogState.segmentsRevealed == 0 then
                button [onClick <| Reveal] [text "Begin"]
            else if blogState.segmentsRevealed < List.length blogState.content.segments then
                button [onClick <| Reveal] [text "Continue"]
            else
                div [][]

    in
    div [class "container", id "container"] [ titleHtml
           , revealedSegmentsHtml
           , revealButtonHtml
           , div [class "bottomGap", id "bottomGap"] []
           ]

coverImageHTML : String -> Html Msg
coverImageHTML coverImage =
    if coverImage == "none" then div[] []
    else div [] [img [class "coverImage", src coverImage] []]

segmentImagesHtml : List String -> Html Msg
segmentImagesHtml images =
    images
    |> List.map (\image ->
        img [src image] []
        )
    |> div [class "segmentImages"]

segmentHtml : Segment -> Html Msg
segmentHtml segment =
    div [class ("fadeIn segment "++segment.textPosition)]
           [ segmentTextHtml segment.text
           , segmentImagesHtml segment.images
           ]

segmentTextHtml : List String -> Html Msg
segmentTextHtml textContent =
    textContent
    |> List.map (\par ->
        p [] [text par]
        )
    |> div [class "segmentText"]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
