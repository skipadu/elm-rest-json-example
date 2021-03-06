module RestJson exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, h3, li, p, text, ul)
import Http
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)



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


type PostsStatus
    = LoadingPosts
    | LoadedPosts (List Post)
    | ErrorPosts


type UsersStatus
    = LoadingUsers
    | LoadedUsers (List User)
    | ErrorUsers


type alias Model =
    { postsStatus : PostsStatus
    , usersStatus : UsersStatus
    }


initialModel : Model
initialModel =
    { postsStatus = LoadingPosts
    , usersStatus = LoadingUsers
    }


initialCmd : Cmd Msg
initialCmd =
    Cmd.batch
        [ postsFromApi
        , usersFromApi
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCmd )



-- UPDATE


type Msg
    = GotPosts (Result Http.Error (List Post))
    | GotUsers (Result Http.Error (List User))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPosts result ->
            case result of
                Ok posts ->
                    ( { model | postsStatus = LoadedPosts posts }, Cmd.none )

                Err _ ->
                    ( { model | postsStatus = ErrorPosts }, Cmd.none )

        GotUsers result ->
            case result of
                Ok users ->
                    ( { model | usersStatus = LoadedUsers users }, Cmd.none )

                Err _ ->
                    ( { model | usersStatus = ErrorUsers }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "RestJson" ]
        , div []
            [ div []
                [ h2 [] [ text "Posts" ]
                , viewPosts model
                ]
            , div []
                [ h2 [] [ text "Users" ]
                , viewUsers model
                ]
            ]
        ]


viewPosts : Model -> Html Msg
viewPosts model =
    case model.postsStatus of
        LoadingPosts ->
            text "Loading..."

        ErrorPosts ->
            text "Error when tried to get posts from API"

        LoadedPosts posts ->
            div [] (List.map (viewPost model) posts)


viewPost : Model -> Post -> Html Msg
viewPost model post =
    div []
        [ h3 [] [ text post.title ]
        , p [] [ text post.body ]
        , div [] [ viewPostAuthor post.userId model ]
        ]


viewPostAuthor : Int -> Model -> Html Msg
viewPostAuthor userId model =
    case model.usersStatus of
        LoadedUsers users ->
            let
                find : (User -> Bool) -> List User -> Maybe User
                find predicate list =
                    case list of
                        [] ->
                            Nothing

                        first :: rest ->
                            if predicate first then
                                Just first

                            else
                                find predicate rest

                postAuthorElement : Maybe User -> Html msg
                postAuthorElement user =
                    case user of
                        Just u ->
                            div [] [ text ("Author: " ++ u.username) ]

                        _ ->
                            text ""
            in
            div [] [ postAuthorElement (find (\user -> user.id == userId) users) ]

        _ ->
            text ""


viewUsers : Model -> Html Msg
viewUsers model =
    case model.usersStatus of
        LoadingUsers ->
            text "Loading users..."

        ErrorUsers ->
            text "Error when tried to get users from API"

        LoadedUsers users ->
            div [] (List.map viewUser users)


viewUser : User -> Html Msg
viewUser user =
    div []
        [ h3 [] [ text user.name ]
        , ul []
            [ li [] [ text user.username ]
            , li [] [ text user.email ]
            ]
        ]



-- HTTP


postsFromApi : Cmd Msg
postsFromApi =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/posts"
        , expect = Http.expectJson GotPosts (list postDecoder)
        }


type alias Post =
    { userId : Int
    , id : Int
    , title : String
    , body : String
    }


postDecoder : Decoder Post
postDecoder =
    succeed Post
        |> required "userId" int
        |> required "id" int
        |> required "title" string
        |> required "body" string


usersFromApi : Cmd Msg
usersFromApi =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/users"
        , expect = Http.expectJson GotUsers (list userDecoder)
        }


type alias Geo =
    { lat : String
    , lng : String
    }


geoDecoder : Decoder Geo
geoDecoder =
    succeed Geo
        |> required "lat" string
        |> required "lng" string


type alias Address =
    { street : String
    , suite : String
    , city : String
    , zipcode : String
    , geo : Geo
    }


addressDecoder : Decoder Address
addressDecoder =
    succeed Address
        |> required "street" string
        |> required "suite" string
        |> required "city" string
        |> required "zipcode" string
        |> required "geo" geoDecoder


type alias Company =
    { name : String
    , catchPhrase : String
    , bs : String
    }


companyDecoder : Decoder Company
companyDecoder =
    succeed Company
        |> required "name" string
        |> required "catchPhrase" string
        |> required "bs" string


type alias User =
    { id : Int
    , name : String
    , username : String
    , email : String
    , address : Address
    , phone : String
    , website : String
    , company : Company
    }


userDecoder : Decoder User
userDecoder =
    succeed User
        |> required "id" int
        |> required "name" string
        |> required "username" string
        |> required "email" string
        |> required "address" addressDecoder
        |> required "phone" string
        |> required "website" string
        |> required "company" companyDecoder
