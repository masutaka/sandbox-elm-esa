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
    , page : Int
    }


type UserState
    = Init
    | Waiting
    | Loaded Posts
    | Failed Http.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Init 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = Input String
    | Send Int
    | Receive (Result Http.Error Posts)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | searchQuery = newInput }, Cmd.none )

        Send newPage ->
            ( { model
                | userState = Waiting
              }
            , Http.request
                { method = "GET"
                , headers = [ Http.header "Authorization" "Bearer elsEF-bG88mXB_KfgG6WKqnY9bayUL0z8m12L3nWLAc" ]
                , url =
                    Url.Builder.crossOrigin
                        "https://api.esa.io"
                        [ "/v1/teams/feedforce/posts" ]
                        [ Url.Builder.string "q" model.searchQuery, Url.Builder.string "page" (String.fromInt newPage) ]
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
    div [ class "layout-outer__main" ]
        [ div [ class "navbar-sub" ]
            [ i [ class "fa fa-folder navbar-sub__category-toggle category-toggle collapsed" ] []
            , div [ class "dropdown nav-team" ]
                [ a [ class "nav-team__link-home" ]
                    [ div [ class "nav-team__thumbnail" ]
                        [ img [ alt "feedforce", src "https://img.esa.io/uploads/production/teams/12180/icon/thumb_ms_5eec31c8c7a67ba0f127a9ca6268bd48.png" ] [] ]
                    , div [ class "nav-team__name hidden-xs" ] [ text "feedforce" ]
                    ]
                , a [ class "dropdown-toggle nav-team__toggle" ]
                    [ i [ class "fa fa-caret-down nav-team__dropdown-icon" ] [] ]
                , ul [ class "nav-team__dropdown dropdown-menu nav" ] []
                ]
            , div [ class "collapse navbar-collapse navbar-search navbar-sub__search" ]
                [ Html.form [ onSubmit (Send 1), class "navbar-form navbar-sub__navbar-form js-search-form" ]
                    [ div [ class "form-group" ]
                        [ div [ class "typeahead__container" ]
                            [ div [ class "typeahead__query" ]
                                [ i [ class "fa fa-search navbar-form__icon" ] []
                                , span [ class "typeahead__cancel-button" ] []
                                , input
                                    [ class "form-control"
                                    , onInput Input
                                    , placeholder "foo category:bar/baz comment:foobar"
                                    , value model.searchQuery
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "body" ]
            [ div [ class "layout-wrapper" ]
                [ div [ class "row post-index-wrapper" ]
                    [ div [ class "main-column" ]
                        [ h2 []
                            [ i [ class "fa fa-file-text-o" ] []
                            , text "Posts"
                            ]
                        , case model.userState of
                            Init ->
                                text ""

                            Waiting ->
                                text "Waiting..."

                            Loaded posts ->
                                div []
                                    [ nav [ class "pagination" ]
                                        [ linkPrevPage posts, currentPage posts, linkNextPage posts ]
                                    , div []
                                        [ ul []
                                            (List.map (\post -> linkPost post) posts.posts)
                                        ]
                                    , nav [ class "pagination" ]
                                        [ linkPrevPage posts, currentPage posts, linkNextPage posts ]
                                    ]

                            Failed e ->
                                div [] [ text (Debug.toString e) ]
                        ]
                    ]
                ]
            ]
        ]


linkPost : Post -> Html msg
linkPost post =
    li []
        [ a [ href post.url, target "_blank" ]
            [ text post.full_name ]
        ]


linkPrevPage : Posts -> Html Msg
linkPrevPage posts =
    case posts.prev_page of
        Just prev_page ->
            span [ class "page" ]
                [ a [ onClick (Send prev_page), href "#" ]
                    [ text (String.fromInt prev_page) ]
                ]

        Nothing ->
            text ""


currentPage : Posts -> Html Msg
currentPage posts =
    span [ class "page current" ]
        [ text (String.fromInt posts.page) ]


linkNextPage : Posts -> Html Msg
linkNextPage posts =
    case posts.next_page of
        Just next_page ->
            span [ class "page" ]
                [ a [ onClick (Send next_page), href "#" ]
                    [ text (String.fromInt next_page) ]
                ]

        Nothing ->
            text ""



-- DATA


type alias Posts =
    { posts : List Post
    , prev_page : Maybe Int
    , next_page : Maybe Int
    , page : Int
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
    D.map4 Posts
        (D.field "posts" (D.list postDecoder))
        (D.maybe (D.field "prev_page" D.int))
        (D.maybe (D.field "next_page" D.int))
        (D.field "page" D.int)


postDecoder : Decoder Post
postDecoder =
    D.map5 Post
        (D.maybe (D.field "category" D.string))
        (D.field "name" D.string)
        (D.field "full_name" D.string)
        (D.field "url" D.string)
        (D.field "tags" (D.list D.string))
