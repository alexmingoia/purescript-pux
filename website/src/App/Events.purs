module App.Events where

import App.Config (config)
import App.Effects (AppEffects)
import App.Routes (Route(..), match)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (last)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Data.String (Pattern(..), drop, split)
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readFile)
import Pux (EffModel, noEffects)
import Text.Markdown.SlamDown (SlamDown)
import Text.Markdown.SlamDown.Parser (parseMd)

data Event
  = PageView Route
  | LoadMarkdown String
  | MarkdownLoaded SlamDown
  | FileNotFound String

type State =
  { title :: String
  , route :: Route
  , url :: String
  , guidepage :: GuidePageState
  , loaded :: Boolean
  }

type GuidePageState =
  { filepath :: String
  , markdown :: Maybe SlamDown
  }

init :: String -> State
init url =
  let route = match url in
    { title: config.title
    , route
    , url
    , guidepage:
      { filepath: drop 6 url
      , markdown: Nothing
      }
    , loaded: false
    }

foldp :: forall e. Event -> State -> EffModel State Event (AppEffects e)
foldp (PageView (Guide url)) st =
  foldp (LoadMarkdown st.guidepage.filepath) st

foldp (PageView (Home url)) st =
  foldp (LoadMarkdown "/../README") st

foldp (PageView route) st =
  noEffects $ st { route = route, loaded = true }

foldp (LoadMarkdown filepath) st =
  { state: st
  , effects: [ liftEff $ do
      let path = "../docs/" <> filepath <> ".md"
      found <- exists path
      case found of
        false -> pure $ Just $ FileNotFound filepath
        true -> do
          buffer <- readFile path
          md <- toString UTF8 buffer
          let sliced = case st.route of
                Home _ -> "# Build purely functional type-safe web applications\n\nPux is" <> (fromMaybe "" $ last $ split (Pattern "Pux is") md)
                _ -> md
          pure $ case (parseMd sliced) of
            Left err -> Just $ FileNotFound filepath
            Right parsed -> Just $ MarkdownLoaded parsed
    ]
  }

foldp (MarkdownLoaded md) st =
  noEffects $ st { loaded = true, guidepage = st.guidepage { markdown = Just md } }

foldp (FileNotFound filepath) st =
  noEffects $ st { loaded = true, route = NotFound st.url }
