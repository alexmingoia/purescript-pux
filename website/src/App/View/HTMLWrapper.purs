module App.View.HTMLWrapper where

import App.Config (config)
import CSS (CSS, margin, padding, px)
import CSS.Render (render, renderedSheet)
import Control.Bind (bind)
import Data.Function (($), (#))
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Pux.Renderer.React (dangerouslySetInnerHTML)
import Text.Smolder.HTML (body, style, div, head, html, link, meta, script, title)
import Text.Smolder.HTML.Attributes (charset, content, href, id, name, rel, src, type')
import Text.Smolder.Markup (text, (!))

bodyStyle :: CSS
bodyStyle = do
  margin (0.0 #px) (0.0 #px) (0.0 #px) (0.0 #px)
  padding (0.0 #px) (0.0 #px) (0.0 #px) (0.0 #px)

htmlWrapper :: ∀ ev. String -> HTML ev
htmlWrapper app_html =
  html do
    head do
      meta ! charset "UTF-8"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1"
      title $ text config.title
      link ! rel "icon" ! type' "image/x-icon" ! href "/favicon.ico"
      link ! rel "stylesheet" ! href "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400|Bitter:700"
      link ! rel "stylesheet" ! type' "text/css" ! href "/atom-one-dark.css"
      style ! dangerouslySetInnerHTML (fromMaybe "" (renderedSheet (render bodyStyle))) $ mempty
    body do
      div ! key "app" ! id "app" ! dangerouslySetInnerHTML app_html $ mempty
      script ! src "/highlight.pack.js" $ mempty
      script ! dangerouslySetInnerHTML "hljs.initHighlightingOnLoad();" $ mempty
