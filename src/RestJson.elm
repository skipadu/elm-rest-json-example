module RestJson exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, li, p, text, ul)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, map3, map4, map5, map8, string)



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
            [ viewPosts model
            , viewUsers model
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
        [ h2 [] [ text post.title ]
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
        [ h2 [] [ text user.name ]
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
    map4
        (\userId id title body -> { userId = userId, id = id, title = title, body = body })
        (field "userId" int)
        (field "id" int)
        (field "title" string)
        (field "body" string)


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
    map2
        (\lat lng -> { lat = lat, lng = lng })
        (field "lat" string)
        (field "lng" string)


type alias Address =
    { street : String
    , suite : String
    , city : String
    , zipcode : String
    , geo : Geo
    }


addressDecoder : Decoder Address
addressDecoder =
    map5
        (\street suite city zipcode geo -> { street = street, suite = suite, city = city, zipcode = zipcode, geo = geo })
        (field "street" string)
        (field "suite" string)
        (field "city" string)
        (field "zipcode" string)
        (field "geo" geoDecoder)


type alias Company =
    { name : String
    , catchPhrase : String
    , bs : String
    }


companyDecoder : Decoder Company
companyDecoder =
    map3
        (\name catchPhrase bs -> { name = name, catchPhrase = catchPhrase, bs = bs })
        (field "name" string)
        (field "catchPhrase" string)
        (field "bs" string)


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
    map8
        (\id name username email address phone website company -> { id = id, name = name, username = username, email = email, address = address, phone = phone, website = website, company = company })
        (field "id" int)
        (field "name" string)
        (field "username" string)
        (field "email" string)
        (field "address" addressDecoder)
        (field "phone" string)
        (field "website" string)
        (field "company" companyDecoder)
