module Web where

import Prelude hiding (div)

import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import LaTeX as LaTeX
import Marked as Marked
-- import SanitizeHtml as Sanitize
import Spork.App as App
import Spork.Html (Html)
import Spork.Html as H
import Spork.Transition (Transition, purely)

type Model = 
  { page :: Page
  , url :: String
  , rawText :: String
  , viewerHtml :: String
  , error :: Maybe String
  , isLoading :: Boolean
  }
data Page
  = LoadingPage
  | InputPage
  | ViewerPage

initModel :: Model
initModel = 
  { page: LoadingPage
  , url: ""
  , rawText: ""
  , error: Nothing
  , viewerHtml: ""
  , isLoading: false
  }

data Action 
  = Initialize
  | Main
  | UpdateURL String
  | UpdateRawText String
  | ValidateAndLoadViewer
  | FileLoaded String
  | LoadError String
  | TextParsed String
  | TextLoaded

data ActionE a 
  = RunInit a
  | RunParse String (String -> a)
  | RunLoadFromUrl String (String -> a) (String -> a)
  | RunLoadHtml String a

foreign import loadHtml :: String -> Effect Unit

update ∷ Model → Action → Transition ActionE Model Action
update model = case _ of
  Initialize -> purely model { page = InputPage }
  Main -> purely model
  UpdateURL text -> purely model { url = text }
  UpdateRawText text -> purely model { rawText = text }
  ValidateAndLoadViewer -> validateAndLoadViewer model'
  FileLoaded text -> validateAndLoadViewer (model' { rawText = text, url = "" })
  LoadError errorMessage -> purely model { isLoading = false, error = Just errorMessage }
  TextParsed viewerHtml -> { model: model { viewerHtml = viewerHtml, page = ViewerPage }, effects: App.lift (RunLoadHtml viewerHtml TextLoaded) }
  TextLoaded -> purely model { isLoading = false }

  where

    validateAndLoadViewer m = case m.url, m.rawText of
      "", "" -> purely m { error = Just "Please enter an URL or paste your input text"}
      url, "" -> { model: m { isLoading = true }, effects: App.lift (RunLoadFromUrl url FileLoaded LoadError) }
      "", text -> { model: m { isLoading = true }, effects: App.lift (RunParse text TextParsed) }
      _, _ -> purely m { error = Just "Please choose only one of the input options" }

    model' = model { error = Nothing, isLoading = false }

run :: ActionE ~> Aff
run effect =
  case effect of
    RunInit next  -> pure next
    RunLoadFromUrl url success fail -> do
      Affjax.get ResponseFormat.string url >>= case _ of
        Right response -> case response.status of
          StatusCode 200 -> pure (success response.body)
          StatusCode code -> pure (fail $ "Server returned error code " <> show code)
        Left error -> pure (fail $ Affjax.printError error)
    RunParse rawText next -> liftEffect do
      result <-
        ( LaTeX.replace
        >=> Marked.parse
        -- >=> Sanitize.sanitize
        ) rawText

      pure (next result)
    RunLoadHtml html next -> liftEffect do
      loadHtml html
      pure next

render ∷ Model → Html Action
render model =
  H.main [ H.classes [ "container" ] ] 
    [ case model.page of
        LoadingPage -> loadingPage
        InputPage -> inputPage
        ViewerPage -> viewerPage
    ]

  where

    loadingPage =
      H.div [] []

    inputPage = 
      H.div []
      [ inputArea true
      , viewerArea false
      ]

    viewerPage = 
      H.div []
      [ inputArea false
      , viewerArea true
      ]

    inputArea visible =
      H.div [ H.classes [ if visible then "visible" else "invisible" ]]
      [ H.nav []
        [ H.ul []
          [ H.li []
            [ H.h3 [] [ H.text "Simple Markdown viewer with LaTeX support" ]
            ]
          ]
        , H.ul []
          [ H.li []
            [ H.a [ H.href "https://github.com/j-nava/markdown-latex-viewer" ] [ H.text "Source" ]
            ]
          , H.li []
            [ H.a [ H.href "https://github.com/j-nava" ] [ H.text "Author" ]
            ]
          ]
        ]
      , H.article []
        [ H.form [] 
          [ H.label [ H.for "input-text"] 
            [ H.text "Raw markdown text"
            , H.textarea 
              [ H.id_ "input-text" 
              , H.onValueInput (H.always UpdateRawText)
              , H.value model.rawText
              ]
            ]
          , H.h4 [] 
            [ H.text "OR" ]
          , H.label [ H.for "url"]
            [ H.text "URL"
            , H.input 
              [ H.type_ H.InputText
              , H.id_ "url"
              , H.onValueInput (H.always UpdateURL)
              , H.value model.url
              ]
            ]
          , H.button 
            [ H.type_ H.ButtonButton 
            , H.onClick (H.always_ ValidateAndLoadViewer)
            , H.attr "aria-busy" submitButtonBusy
            ] 
            [ H.text submitButtonText ]
          , maybe (H.empty) (\errorMessage -> H.h6 [ H.classes [ "secondary" ] ] [ H.text errorMessage ]) model.error
          ]
        ]
      ]

      where

        submitButtonText = if model.isLoading then "Please wait..." else "Submit"
        submitButtonBusy = if model.isLoading then "true" else "false"

    viewerArea visible =
      H.div [ H.classes [ if visible then "visible" else "invisible" ]]
      [ H.div [ H.id_ "divText"] [] ]

app :: App.App ActionE (Const Void) Model Action
app = { render, update, subs: const mempty, init: { model: initModel, effects: App.lift (RunInit Initialize) } }

handleErrors :: Error -> Effect Unit
handleErrors _ = pure unit
