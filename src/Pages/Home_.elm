module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html
import Html.Events as E
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg exposing (Msg(..))
import Util.Toast as Toast
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp
    | ShowToast
    | ToastMsg Toast.Msg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        ShowToast ->
            ( model
            , Effect.showToast
                (Toast.init
                    |> Toast.setTitle "Toast title"
                    |> Toast.setContent
                        (Html.div [] [ Html.text "This is the toast content" ])
                )
            )

        ToastMsg tmsg ->
            ( model
            , Effect.toastMsg tmsg
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Pages.Home_"
    , body =
        [ Html.div []
            [ Html.div [] [ Html.text "This is the body of Pages.Home_" ]
            , Html.button [ E.onClick ShowToast ] [ Html.text "Show toast" ]
            ]

        -- this is where the problem is
        -- not sure how to fix it
        , Toast.viewList shared.toasts
            |> Html.map
                ToastMsg
        ]
    }
