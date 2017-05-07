import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main = Html.beginnerProgram { model = model
            , view = view
            , update = update }

-- MODEL

type alias Model = User

type alias User = {
    name: String
    , email: String
    , role: String
    , tags: String
    , loc: String
    , phno: Int
}

model : Model
model = {
     name: ""
    , email: ""
    , role: ""
    , tags: ""
    , loc: ""
    , phno: 0
}