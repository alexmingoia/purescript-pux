module Server where

import App.Config (config)
import App.Events (Event(PageView), foldp, init)
import App.Effects (AppEffects)
import App.View.HTMLWrapper (htmlWrapper)
import App.View.Layout (view)
import App.Routes (Route(..))
import Control.Bind (bind)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, message)
import Control.Monad.Eff.Ref (REF)
import Data.Function (($), (<<<))
import Data.Functor ((<$>))
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Node.Express.App (listenHttp, use, useOnError)
import Node.Express.Handler (Handler)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getOriginalUrl)
import Node.Express.Response (send, sendJson, setStatus)
import Node.Express.Types (EXPRESS)
import Node.HTTP (Server)
import Node.Process (PROCESS, lookupEnv)
import Pux (CoreEffects, start, waitState)
import Pux.Renderer.React (renderToString, renderToStaticMarkup)
import Signal (constant)

-- | Express route handler which renders the application.
appHandler :: forall e. Handler (CoreEffects (AppEffects e))
appHandler = do
  url <- getOriginalUrl

  let state = init url

  app <- liftEff $ start
    { initialState: state
    , view
    , foldp
    , inputs: [constant (PageView state.route)]
    }

  loaded <- liftAff $ waitState (\s -> s.loaded) app

  html <- liftEff $ do
    app_html <- renderToString app.markup
    renderToStaticMarkup $ constant (htmlWrapper app_html)

  -- | Set proper response status
  case state.route of
    (NotFound _) -> setStatus 404
    _ -> setStatus 200

  send html

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
  setStatus 500
  sendJson {error: message err}

-- | Starts server (for production).
main :: Eff (CoreEffects (AppEffects (ref :: REF, express :: EXPRESS, console :: CONSOLE, process :: PROCESS))) Server
main = do
  let parseInt = fromMaybe 0 <<< fromString

  port <- (parseInt <<< fromMaybe "3000") <$> lookupEnv "PORT"

  let app = do
        use         (static config.public_path)
        use         appHandler
        useOnError  errorHandler

  listenHttp app port \_ ->
    log $ "Listening on " <> show port
