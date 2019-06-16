module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)


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
    { input : String
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
            ( { model | input = newInput }, Cmd.none )

        Send ->
            ( { model
                | input = ""
                , userState = Waiting
              }
            , Http.request
                { method = "GET"
                , headers = [ Http.header "Authorization" "Bearer elsEF-bG88mXB_KfgG6WKqnY9bayUL0z8m12L3nWLAc" ]
                , url = "https://api.esa.io/v1/teams/feedforce/posts"

                -- , url = "https://api.esa.io/v1/teams/" ++ model.input ++ "/posts"
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
                , placeholder "GitHub name"
                , value model.input
                ]
                []
            , button
                [ disabled
                    ((model.userState == Waiting)
                        || String.isEmpty (String.trim model.input)
                    )
                ]
                [ text "Submit" ]
            ]
        , case model.userState of
            Init ->
                text ""

            Waiting ->
                text "Waiting..."

            Loaded posts ->
                ul []
                    (List.map (\post -> linkPost post) posts.posts)

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
