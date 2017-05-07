port module App exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
--import Http
--import Json.Decode as Decode exposing (..)
--import Json.Encode as Encode exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram 
    { model = init, view = view, update = update }



{- 
MODEL
* Model type 
* Initialize model with empty values
-}


type alias Model =
    { username : String
    , password : String
    , email : String
    , phno : String
    , role : String
    , tags :String
    , errorMsg : String
    , screen : String
}

init : Model
init =
    Model "Ajay" "1234" "" "" "" "" "" "1"

-- Array of users
user1 = Model "Ajay" "1234" "ajay@gmail.com" "789654123" "teacher" "java, c, c++" "" "1"
user2 = Model "Jeorge" "1234" "jeorge@gmail.com" "789654123" "seeker" "java" "" "1"
user3 = Model "Hussain" "1234" "hussain@gmail.com" "789654123" "teacher" "javascript, html" "" "1"
user4 = Model "Javed" "1234" "javed@gmail.com" "789654123" "teacher" "python" "" "1"

userList = [user1, user2, user3, user4]


checkUser : List Model -> String -> String -> Bool
checkUser models name pass =
    case models of
        [] -> False
        (x::xs) -> 
            if x.username == name && x.password == pass
            then True
            else checkUser xs name pass

getUser : List Model -> String -> Model
getUser models name =
    case models of
        [] -> Model "Ajay" "1234" "" "" "" "" "" "1"
        (x::xs) -> 
            if x.username == name
            then x
            else getUser xs name


-- Messages 


type Msg = SetUsername String
            | SetPassword String
            | ClickRegisterUser
            | ClickLogIn
            | LogOut
            | ClickUser String


-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetUsername username ->
            { model | username = username }

        SetPassword password ->
            { model | password = password }

        ClickRegisterUser ->
            { model | errorMsg = "" }

        ClickUser name ->
            {  model | username = (getUser userList name).username, screen = "3" }

        ClickLogIn ->
            if checkUser userList model.username model.password --model.username == "Ajay" && model.password == "1234"
            then { model | errorMsg = "", screen = "2" }
            else { model | errorMsg = "Invalid Username or Password", screen = "1" }
    
        LogOut ->
            { model | username = "", password = "", screen = "1" }


{-
VIEW
* Hide sections of view depending on authenticaton state of model
* Get a quote
* Log In or Register
-}


--Which screen to display
type Screens = One | Two | Three

view : Model -> Html Msg
view model = 
    case model.screen of
        "1" -> loginPage model
        "2" -> secondPage model
        "3" -> thirdPage model
        _ -> Debug.crash "Help"




loginPage : Model -> Html Msg
loginPage model = 
    let
        showError : String
        showError =
            if String.isEmpty model.errorMsg then
                "hidden"
            else
                ""
    in
        div [ class "container-mod container" ]
            [   div [ class "jumbotron-mod jumbotron text-left" ]
                    [ -- Login/Register form or user greeting
                    div [ id "form" ]
                    [ h2 [ class "text-center" ] [ text "Log In" ]
                    , p [ class "text-center help-block" ] [ text "Please Log In." ]
                    , div [ class showError ]
                        [ div [ class "alert alert-danger" ] [ text model.errorMsg ]
                        ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "username" ] [ text "Username:" ]
                    , input [ id "username", type_ "text", class "form-control", Html.Attributes.value model.username, onInput SetUsername ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "password" ] [ text "Password:" ]
                    , input [ id "password", type_ "password", class "form-control", Html.Attributes.value model.password, onInput SetPassword ] []
                    ]
                    ]
                    , div [ class "text-center" ] [
                    button [ class "btn btn-primary", onClick ClickLogIn ] [ text "Log In" ]
                        --, button [ class "btn btn-link", onClick ClickRegisterUser ] [ text "Register" ]
                    ]
                    ]]]

secondPage : Model -> Html Msg
secondPage model = 
    let 
        loggedUser = model.username
    in
    div [ class "container" ] 
        [ div [ class "row"] 
            [ p [] [ text model.username ]
            , button [ class "btn btn-primary", onClick LogOut ] [ text "Logout" ]
            ]
        , div [ class "col-xs-12 col-md-6"] 
            [ p [] [text "Hello world"] ]
            , div [ class "col-xs-12 col-md-6" ]
                [ div [ class "row col-xs-12 col-md-12"] 
                    [ div [class "list-group"] 
                        [ a [class "list-group-item list-group-item-action align-items-start", onClick (ClickUser "Ajay")] 
                            [ h4 [] [ text "Ajay (Teacher)" ]
                            , p [] [ text "Courses : Java, C, C++"]
                            ]
                        , a [class "list-group-item list-group-item-action align-items-start", onClick (ClickUser "Ajay") ] 
                            [ h4 [] [ text "Jeorge (Teacher)" ]
                            , p [] [ text "Courses : Java"]
                            ]
                        , a [class "list-group-item list-group-item-action align-items-start", onClick (ClickUser "Ajay") ] 
                            [ h4 [] [ text "Hussain (Seeker)" ]
                            , p [] [ text "Interested in : Java"]
                            ]
                        , a [class "list-group-item list-group-item-action align-items-start", onClick (ClickUser "Ajay") ] 
                            [ h4 [] [ text "Javed (Teacher)" ]
                            , p [] [ text "Courses : Python"]
                            ]
                        ]
                ]
            ]
        ]

thirdPage : Model -> Html Msg
thirdPage model = 
    div [ class "container-fuild" ] 
        [ div [ class "row"] 
            [ h4 [ class "col-xs-8" ] [ text model.username ]
            , button [ class "btn btn-primary col-xs-6 col-xs-offset-2 ", onClick LogOut ] [ text "Logout" ]
            ]
        , div [ class "row"]
            [  ]
        ] 


--navbar : String -> Html Msg
--navbar name = 
--    nav [  ] 
--        []
--renderDiv : List Model -> Html Msg
--renderDiv models =
--    case models of
--        [] -> div [ class "row col-xs-12" ] [ p [] [ text "No users around" ] ]
--        (x::xs) -> 
--            div [ class "row col-xs-12" ] [ p [] [ text x.username ] ](renderDiv xs)


--div [ class "jumbotron text-center"]
--              [ p [] [ text "hello world"] ]  
--        , div [ class "text-center" ] 
--
--              [button [ class "btn btn-primary", onClick LogOut ] [ text "Logout" ]
--              ]






--div [ class "col-xs-12 col-md-6"] 
--                [ p [] [text "Hello world"] ]
--            , div [ class "col-xs-12 col-md-6" ]
--                [ div [ class "row card card-primary card-inverse" ] 
--                    [ div [class "card-title"] [ text "Ajay" ] ]
--                , div [ class "row col-xs-12" ] 
--                    [ p [] [ text "Jeorge" ] ]
--                , div [ class "row col-xs-12" ] 
--                    [ p [] [ text "Hussain" ] ]
--                , div [ class "row col-xs-12" ] 
--                    [ p [] [ text "Javed" ] ]
--                ]