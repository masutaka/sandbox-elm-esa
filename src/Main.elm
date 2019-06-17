module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Url.Builder


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { searchQuery : String
    , userState : UserState
    }


type UserState
    = Init
    | Waiting
    | Loaded Posts
    | Failed Http.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Init
    , Cmd.none
    )



-- UPDATE


type Msg
    = Input String
    | Send
    | Receive (Result Http.Error Posts)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | searchQuery = newInput }, Cmd.none )

        Send ->
            ( { model
                | searchQuery = ""
                , userState = Waiting
              }
            , Http.request
                { method = "GET"
                , headers = [ Http.header "Authorization" "Bearer elsEF-bG88mXB_KfgG6WKqnY9bayUL0z8m12L3nWLAc" ]
                , url =
                    Url.Builder.crossOrigin
                        "https://api.esa.io"
                        [ "/v1/teams/feedforce/posts" ]
                        [ Url.Builder.string "q" model.searchQuery, Url.Builder.string "page" "1" ]
                , body = Http.emptyBody
                , timeout = Nothing
                , tracker = Nothing
                , expect = Http.expectJson Receive postsDecoder
                }
            )

        Receive (Ok posts) ->
            ( { model | userState = Loaded posts }, Cmd.none )

        Receive (Err e) ->
            ( { model | userState = Failed e }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Send ]
            [ input
                [ onInput Input
                , autofocus True
                , placeholder "foo category:bar/baz comment:foobar"
                , value model.searchQuery
                ]
                []
            , button
                [ disabled (model.userState == Waiting) ]
                [ text "Submit" ]
            ]
        , case model.userState of
            Init ->
                text ""

            Waiting ->
                text "Waiting..."

            Loaded posts ->
                div []
                    [ ul []
                        (List.map (\post -> linkPost post) posts.posts)
                    , text (String.fromInt (Maybe.withDefault 1 posts.next_page))
                    ]

            Failed e ->
                div [] [ text (Debug.toString e) ]
        ]


linkPost : Post -> Html msg
linkPost post =
    li []
        [ a [ href post.url, target "_blank" ]
            [ text post.full_name ]
        ]



-- DATA


type alias Posts =
    { posts : List Post
    , next_page : Maybe Int
    }


type alias Post =
    { category : Maybe String
    , name : String
    , full_name : String
    , url : String
    , tags : List String
    }


postsDecoder : Decoder Posts
postsDecoder =
    D.map2 Posts
        (D.field "posts" (D.list postDecoder))
        (D.maybe (D.field "next_page" D.int))


postDecoder : Decoder Post
postDecoder =
    D.map5 Post
        (D.maybe (D.field "category" D.string))
        (D.field "name" D.string)
        (D.field "full_name" D.string)
        (D.field "url" D.string)
        (D.field "tags" (D.list D.string))
