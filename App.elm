port module App exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (..)

import Regex


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

-- Array of defualt users
user1 = Model "Ajay" "1234" "ajay@gmail.com" "+91789654123" "teacher" "Java, C, C++" "" "1" ""
user2 = Model "Jeorge" "1234" "jeorge@gmail.com" "+91789654123" "seeker" "Java" "" "1" ""
user3 = Model "Hussain" "1234" "hussain@gmail.com" "+91789654123" "teacher" "Javascript, HTML, ELM, Haskell" "" "1" ""
user4 = Model "Javed" "1234" "javed@gmail.com" "+918892585434" "teacher" "Python, R" "" "1" ""

userList = [user1, user2, user3, user4]


--check whether given user is already present in user list
checkUser : List Model -> String -> String -> Bool
checkUser models name pass =
    case models of
        [] -> False
        (x::xs) -> 
            if x.username == name && x.password == pass
            then True
            else checkUser xs name pass


--get user details for the given user
getUser : List Model -> String -> Model
getUser models name =
    case models of
        [] -> Model "" "" "" "" "" "" "" "1" ""
        (x::xs) -> 
            if x.username == name
            then x
            else getUser xs name

--validate the given user input for registraiton of new users
--regex for matching patterns
patternName = Regex.regex "[a-zA-Z_ ]+" 

patternPhno = Regex.regex "[+][0-9]{12}" 

patternTags = Regex.regex "[a-zA-Z0-9_, ]+" 

patternEmail = Regex.regex "[a-zA-Z0-9_]+@[a-zA-Z0-9-]+[.][a-zA-Z0-9-]+"

--validate user input based on regex matching
validateUser : Model -> Bool
validateUser model = 
    if Regex.contains patternName model.username &&
         Regex.contains patternPhno model.phno &&
            Regex.contains patternTags model.tags &&
                Regex.contains patternEmail model.email
    then True
    else False

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
            { model | screen = "1" }

        Home -> 
            { model | screen = "2" }

        ClickRegister -> 
            if checkUser userList model.username model.password
            then { model | errorMsg = "Username taken.", screen = "0" }
            else if validateUser model
            then { model | errorMsg = "", screen = "2" }
            else { model | errorMsg = "Enter all details correctly.", screen = "0" }

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
* Hide sections of view depending on screen value
* Log In or Register
-}


--Which screen to display
type Screens = One | Two | Three


--View
--view is decided by what is the value of screen
view : Model -> Html Msg
view model = 
    case model.screen of
        "0" -> signUpPage model
        "1" -> loginPage model
        "2" -> secondPage model
        "3" -> thirdPage model
        _ -> Debug.crash "Help"


-- login page view
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
            [ div [class "row text-center"] [ h1 [] [text "Meet-Ur-Tutor"] ]  
            , div [ class "jumbotron-mod jumbotron text-left" ]
                    [ 
                    div [ id "form" ]
                    [ h2 [ class "text-center" ] [ text "Log In" ]
                    , p [ class "text-center help-block" ] [ text "Please Log In." ]
                    , div [ class showError ]
                        [ div [ class "alert alert-danger" ] [ text model.errorMsg ]
                        ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "username" ] [ text "Username:" ]
                    , input [ id "username", type_ "text", class "form-control", onInput SetUsername ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "password" ] [ text "Password:" ]
                    , input [ id "password", type_ "password", class "form-control", onInput SetPassword ] []
                    ]
                    ]
                    , div [ class "text-center" ] [
                    button [ class "btn btn-primary", onClick ClickLogIn ] [ text "Log In" ]
                    , button [ class "btn btn-link", onClick RegisterUserPage ] [ text "Register" ]
                    ]
                    ]]]


--sign up page view
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
            [   div [class "row text-center"] [ h1 [] [text "Meet-Ur-Tutor"] ]  
            , div [ class "jumbotron-mod jumbotron text-left" ]
                    [
                    div [ id "form" ]
                    [ h2 [ class "text-center" ] [ text "Sign Up" ]
                    , p [ class "text-center help-block" ] [ text "Please Sign Up by filling your details." ]
                    , div [ class showError ]
                        [ div [ class "alert alert-danger" ] [ text model.errorMsg ]
                        ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "username" ] [ text "Username:" ]
                    , input [ id "username", type_ "text", class "form-control", onInput SetUsername ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "password" ] [ text "Password:" ]
                    , input [ id "password", type_ "password", class "form-control", onInput SetPassword ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "phno" ] [ text "Ph. No.:" ]
                    , input [ id "phno", type_ "text", class "form-control", onInput SetPhNo ] []
                    ]
                    ]
                    , div [ class "form-group row" ]
                    [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "email" ] [ text "Email:" ]
                    , input [ id "email", type_ "text", class "form-control", onInput SetEmail ] []
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
                    , input [ id "tags", type_ "text", class "form-control",  onInput SetTags ] []
                    ]
                    ]

                    , div [ class "text-center" ] [
                    button [ class "btn btn-link", onClick LogInUserPage ] [ text "Log In" ]
                    , button [ class "btn btn-primary", onClick ClickRegister ] [ text "Register" ]
                    ]
                    ]]]


--wrapper for google-map to correctly implement map apis
mapWrapper : List (Attribute a) -> List (Html a) -> Html a
mapWrapper =
    Html.node "map-wrapper"


--second page view, page where nearby users are displayed
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
                ]
                []
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
                            , p [] [ text "Interested in : Javascript, HTML, ELM, Haskell"]
                            ]
                        , a [class "list-group-item list-group-item-action align-items-start", onClick (ClickUser "Javed") ] 
                            [ h4 [] [ text "Javed (Teacher)" ]
                            , p [] [ text "Courses : Python, R"]
                            ]
                        ]
                ]
            ]
        ]


--third page view, page where selected person's details are displayed
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
                [ div [ class "" ] 
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
                    ]
                    []
                ]
            ]
        ] 


--navigation bar wrapped up neatly
navbar : String -> Html Msg
navbar name = 
    nav [ class "navbar" ] 
        [ div [ class "container-fluid" ]
            [ ul [ class "nav navbar-nav navbar-left"] 
                [ li [][ a [] [ text name ]  ]
                ]
            , div [ class "navbar-header" ]
                [ a [ class "navbar-brand" ]
                    [ text "Meet-Ur-Tutor" ]
                ]
            , ul [ class "nav navbar-nav navbar-right"] 
                [ li [] [ a [onClick Home] [ text "Home" ] ]
                , li [] [ a [onClick LogOut] [ text "Logout" ] ]
                ]
            ]
        ]
