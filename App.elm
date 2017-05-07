port module App exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (..)
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
    , checkout : String
}

init : Model
init =
    Model "Ajay" "1234" "" "" "" "" "" "1" ""

-- Array of users
user1 = Model "Ajay" "1234" "ajay@gmail.com" "789654123" "teacher" "java, c, c++" "" "1" ""
user2 = Model "Jeorge" "1234" "jeorge@gmail.com" "789654123" "seeker" "java" "" "1" ""
user3 = Model "Hussain" "1234" "hussain@gmail.com" "789654123" "teacher" "javascript, html" "" "1" ""
user4 = Model "Javed" "1234" "javed@gmail.com" "789654123" "teacher" "python" "" "1" ""

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
        [] -> Model "Ajay" "1234" "" "" "" "" "" "1" ""
        (x::xs) -> 
            if x.username == name
            then x
            else getUser xs name

--appendUser : Model -> String
--appendUser model = 
--    model::userList

-- Messages 


type Msg = SetUsername String
            | SetPassword String
            | RegisterUserPage
            | ClickLogIn
            | LogOut
            | ClickUser String
            | Home
            | LogInUserPage
            | ClickRegister
            | SetPhNo String
            | SetTags String
            | SetEmail String
            | SetRole String


-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetUsername username ->
            { model | username = username }

        SetPassword password ->
            { model | password = password }

        RegisterUserPage ->
            { model | errorMsg = "", screen = "0" }

        ClickUser name ->
            {  model | checkout = (getUser userList name).username, screen = "3" }

        ClickLogIn ->
            if checkUser userList model.username model.password
            then { model | errorMsg = "", screen = "2" }
            else { model | errorMsg = "Invalid Username or Password", screen = "1" }
    
        LogOut ->
            { model | username = "", password = "", screen = "1" }

        Home -> 
            { model | screen = "2" }

        ClickRegister -> 
            if checkUser userList model.username model.password
            then { model | errorMsg = "Username taken", screen = "0" }
            else { model | errorMsg = "", screen = "2" }

        LogInUserPage ->
            { model | screen = "1" }

        SetPhNo phno ->
            { model | phno = phno }

        SetTags tags ->
            { model | tags = tags }

        SetEmail val ->
            { model | email = val }

        SetRole val ->
            { model | role = val }



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
        "0" -> signUpPage model
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
                    , button [ class "btn btn-link", onClick RegisterUserPage ] [ text "Register" ]
                    ]
                    ]]]

signUpPage : Model -> Html Msg
signUpPage model = 
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
                    [ h2 [ class "text-center" ] [ text "Sign Up" ]
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
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "phno" ] [ text "Ph. No.:" ]
                    , input [ id "phno", type_ "text", class "form-control", Html.Attributes.value model.phno, onInput SetPhNo ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "email" ] [ text "Email:" ]
                    , input [ id "email", type_ "text", class "form-control", Html.Attributes.value model.email, onInput SetEmail ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "role" ] [ text "Role:   " ]
                    , input [ id "role", type_ "radio", class "", name "role", Html.Attributes.value "Teacher", checked True] []
                    , text " Teacher  "
                    , input [ id "role", type_ "radio", class "", name "role", Html.Attributes.value "Seeker" ] []
                    , text " Seeker  "
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "tags" ] [ text "Keywords:" ]
                    , input [ id "tags", type_ "text", class "form-control", Html.Attributes.value model.tags, onInput SetTags ] []
                    ]
                    ]

                    , div [ class "text-center" ] [
                    button [ class "btn btn-link", onClick LogInUserPage ] [ text "Log In" ]
                    , button [ class "btn btn-primary", onClick ClickRegister ] [ text "Register" ]
                    ]
                    ]]]

mapWrapper : List (Attribute a) -> List (Html a) -> Html a
mapWrapper =
    Html.node "map-wrapper"


secondPage : Model -> Html Msg
secondPage model = 
    let 
        loggedUser = model.username
    in
    div [ class "container" ] 
        [ div [ class "row"] [ navbar model.username ]
        , div [ class "col-xs-12 col-md-6"] 
            [  mapWrapper
                [ attribute "latitude" "12.942149"
                , attribute "longitude" "77.622002"
                --, attribute "drag-events" "true"
                ]
                []
            --p [] [text "Hello world"] 
            ]
            , div [ class "col-xs-12 col-md-6" ]
                [ div [ class "row col-xs-12 col-md-12"] 
                    [ div [class "list-group"] 
                        [ a [class "list-group-item list-group-item-action align-items-start", onClick (ClickUser "Ajay")] 
                            [ h4 [] [ text "Ajay (Teacher)" ]
                            , p [] [ text "Courses : Java, C, C++"]
                            ]
                        , a [class "list-group-item list-group-item-action align-items-start", onClick (ClickUser "Jeorge") ] 
                            [ h4 [] [ text "Jeorge (Seeker)" ]
                            , p [] [ text "Courses : Java"]
                            ]
                        , a [class "list-group-item list-group-item-action align-items-start", onClick (ClickUser "Hussain") ] 
                            [ h4 [] [ text "Hussain (Teacher)" ]
                            , p [] [ text "Interested in : Java"]
                            ]
                        , a [class "list-group-item list-group-item-action align-items-start", onClick (ClickUser "Javed") ] 
                            [ h4 [] [ text "Javed (Teacher)" ]
                            , p [] [ text "Courses : Python"]
                            ]
                        ]
                ]
            ]
        ]

--onChange : Msg -> Msg
--onChange msg = 
--    msg

thirdPage : Model -> Html Msg
thirdPage model = 
    let 
        member = 
            getUser userList model.checkout

        showteacher = 
            if member.role == "teacher"
            then ""
            else "hidden"

        showseeker = 
            if member.role == "seeker"
            then ""
            else "hidden"
    in
    div [ class "container" ] 
        [ div [ class "row"] [ navbar model.username ]
        , div [ class "row"]
            [ h2 [ class "jumbotron"] [ text model.checkout ] ]
        , div [ class "row" ]
            [ div [class "col-md-6"] 
                [ div [ class "text-left" ] 
                    [   table [ style [("cell-padding", "10px")]] 
                            [   tr [] 
                                    [ td [class showteacher] [ h4 [] [text "Teaches : " ]]
                                    , td [class showseeker] [ h4 [] [text "Interested In : "]]
                                    , td [] [ h5 [] [text member.tags] ]
                                    ]
                            ,   tr []
                                    [   td [] [ h4 [] [ text "Contact : "]  ] 
                                    , td [] [ h4 [] [ a[href ("whatsapp://send?text=Hi there!&phone=" ++ (toString member.phno)), class "btn btn-link"] [ text "Ping me on whatsapp!!" ] ]]
                                    ] 
                            , tr []
                                [ td [] [ h4 [] [text "Email : "] ]
                                , td [] [ a [href ("mailto:" ++ member.email), class "btn btn-link"] [text member.email]] ]   
                            ]
                    ]
                ]
            , div [ class "col-md-6" ] 
                [ mapWrapper
                    [ attribute "latitude" "12.942149"
                    , attribute "longitude" "77.622002"
                    --, attribute "drag-events" "true"
                    ]
                    []
                ]
            ]
        ] 





navbar : String -> Html Msg
navbar name = 
    nav [ class "navbar" ] 
        [ div [ class "container-fluid" ]
            [ div [ class "navbar-header" ]
                [ a [ class "navbar-brand" ]
                    [ text name ]
                ]
            , ul [ class "nav navbar-nav"] 
                [ li [] [ a [onClick Home] [ text "Home" ] ]
                , li [] [ a [onClick LogOut] [ text "Logout" ] ]
                ]
            ]
        ]
