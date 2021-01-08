module RestJson exposing (main)

import Browser
import Html exposing (Html, div, h1, text)
import Http
import Json.Decode exposing (Decoder, field, int, list, map4, string)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Status
    = Loading
    | Loaded (List Post)
    | Error


type alias Model =
    { status : Status
    , placeholder : String
    }


initialModel : Model
initialModel =
    { status = Loading
    , placeholder = "Just a placeholder"
    }


initialCmd : Cmd Msg
initialCmd =
    postsFromApi


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCmd )



-- UPDATE


type Msg
    = GotPosts (Result Http.Error (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPosts result ->
            case result of
                Ok posts ->
                    ( { model | status = Loaded posts }, Cmd.none )

                Err _ ->
                    ( { model | status = Error }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Post =
    { userId : Int
    , id : Int
    , title : String
    , body : String
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "RestJson" ]
        , div []
            [ viewPosts model ]
        ]


viewPosts : Model -> Html Msg
viewPosts model =
    case model.status of
        Loading ->
            text "Loading..."

        Error ->
            text "Error when tried to get posts from API"

        -- TODO: Use map to create elements for each and every posts
        Loaded posts ->
            div [] (List.map viewPost posts)


viewPost : Post -> Html Msg
viewPost post =
    div []
        [ text ("Title" ++ post.title) ]



-- HTTP


postsFromApi : Cmd Msg
postsFromApi =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/posts"
        , expect = Http.expectJson GotPosts (list postDecoder)
        }


postDecoder : Decoder Post
postDecoder =
    map4
        (\userId id title body -> { userId = userId, id = id, title = title, body = body })
        (field "userId" int)
        (field "id" int)
        (field "title" string)
        (field "body" string)
