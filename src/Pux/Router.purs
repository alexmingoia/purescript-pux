-- | Routing means many different things. In Pux, routing is a set of
-- | applicatives for matching URLs to data types, along with a signal for
-- | location changes.
-- |
-- | First, URL changes need to be mapped to an action or actions. We also need
-- | a data type for routes that can contain parameter and query data:
-- |
-- | ```purescript
-- | data Action = PageView Route
-- |
-- | data Route = Home | Users | User Int | NotFound
-- | ```
-- |
-- | We also need a function that constructs a route action from a url, which we
-- | build using routing applicatives and pass to `router` to return the
-- | matched route:
-- |
-- | ```purescript
-- | match :: String -> Action
-- | match url = PageView $ fromMaybe NotFound $ router url
-- |   Home <$ end
-- |   <|>
-- |   Users <$ (lit "users") <* end
-- |   <|>
-- |   User <$> (lit "users" *> int) <* end
-- | ```
-- |
-- | `Pux.Router` provides applicatives `lit`, `str`, `num`, `int`, `bool`, `param`,
-- | `params`, `any`, and `end` for mapping url parts to route values.
-- |
-- | As you can see above, `lit` matches a literal string, and its value is ignored.
-- | `int` matches the second part of `/users/123` to the integer value of `User`.
-- | `end` matches the end of the URL.
-- |
-- | Now that we have a function for making a route from a url, we can map it over
-- | the url signal provided by `sampleUrl`:
-- |
-- | ```purescript
-- | main = do
-- |   urlSignal <- sampleUrl
-- |   let routeSignal = urlSignal ~> match
-- |
-- |   renderToDOM (ElementId "app") =<< app
-- |     { state: { currentRoute: Home }
-- |     , update: update
-- |     , view: home
-- |     , inputs: [routeSignal]
-- |     }
-- | ```
-- |
-- | Everytime the location changes, we can update our state with the current route,
-- | which includes any captured path or query parameters:
-- |
-- | ```purescript
-- | update :: forall eff. Update eff State Action
-- | update action state input = case action of
-- |   (PageView route) ->
-- |     { state: { currentRoute: route }
-- |     , effects: []
-- |     }
-- | ```
-- |
-- | Finally, we might want to create links to our routes and show different
-- | views for them. The `link` attribute can be used to push a new location to
-- | HTML5 history, and a simple case expression is used to determine the
-- | correct view:
-- |
-- | ```purescript
-- | view :: View State
-- | view state children = div $ do
-- |   div $ case state.currentRoute of
-- |     Home -> h1 $ text "Home"
-- |     Users -> h1 $ text "Users"
-- |     (User id )-> h1 $ text ("User: " ++ show id)
-- |     _ -> h1 $ text "Not Found"
-- |   ul $ do
-- |     li $ a ! link "/" $ text "Home"
-- |     li $ a ! link "/users" $ text "Users"
-- |     li $ a ! link "/users/123" $ text "User 123"
-- |     li $ a ! link "/foobar" $ text "Not found"
-- | ```
-- |
-- | See it all together in the [routing example](../../examples/routing/).

module Pux.Router
  ( Match()
  , RoutePart(..)
  , sampleUrl
  , navigate
  , link
  , router
  , lit
  , str
  , num
  , int
  , bool
  , param
  , params
  , any
  , end
  ) where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Alt
import Control.Plus
import Control.MonadPlus (guard)
import Data.Maybe
import qualified Data.String as S
import Data.Traversable (traverse)
import Data.Int (fromString)
import qualified Data.Array as A
import Data.List
import Data.Tuple (Tuple(..), fst, snd)
import qualified Data.Map as M
import DOM (DOM())
import Pux.React.Types (Event())
import Pux.View (Attrs(), Handler(..))
import Pux.DOM.HTML.Attributes (onClick, href)
import Global (readFloat, isNaN)
import Signal (constant, Signal())

foreign import sampleUrlFF :: forall eff c.
                              (c -> Signal c) ->
                              Eff (dom :: DOM | eff) (Signal String)

foreign import pushStateFF :: forall eff ev.
                             String ->
                             ev ->
                             Eff (dom :: DOM | eff) Unit

-- | Returns a signal containing the current window location path and query.
sampleUrl :: forall eff. Eff (dom :: DOM | eff) (Signal String)
sampleUrl = sampleUrlFF constant

-- | Creates an attribute that pushes new location to HTML5 history.
-- |
-- | ```purescript
-- | a ! route "/" $ text "Home"
-- | ```
link :: String -> Attrs
link path = hrefAttrs <> onClickAttrs
  where
  hrefAttrs = href path
  onClickAttrs = onClick $ navigate path

-- | Handler that pushes new location to HTML5 history.
-- | `ev.preventDefault()` is called if the event target is an `A` element.
navigate :: forall action eff. String -> Handler Event action (dom :: DOM | eff)
navigate path = Handler Nil $ singleton (\ev -> pushStateFF path ev)

data RoutePart = Path String | Query (M.Map String String)
type Route = List RoutePart

newtype Match a = Match (Route -> Maybe (Tuple Route a))

end :: Match Unit
end = Match $ \r ->
  case r of
    Cons (Query m) Nil -> Just $ Tuple Nil unit
    Nil -> Just $ Tuple Nil unit
    _ -> Nothing

lit :: String -> Match Unit
lit part = Match $ \r ->
  case r of
    Cons (Path p) ps | p == part -> Just $ Tuple ps unit
    _ -> Nothing

num :: Match Number
num = Match $ \r ->
  case r of
    Cons (Path p) ps ->
      let res = readFloat p in
      if isNaN res then
        Nothing
      else
        Just $ Tuple ps res
    _ -> Nothing

int :: Match Int
int = Match $ \r ->
  case r of
    Cons (Path p) ps -> maybe Nothing (Just <<< Tuple ps) $ fromString p
    _ -> Nothing

bool :: Match Boolean
bool = Match $ \r ->
  case r of
    Cons (Path p) ps | p == "true" -> Just $ Tuple ps true
    Cons (Path p) ps | p == "false" -> Just $ Tuple ps false
    _ -> Nothing

str :: Match String
str = Match $ \r ->
  case r of
    Cons (Path p) ps -> Just $ Tuple ps p
    _ -> Nothing

param :: String -> Match String
param key = Match $ \r ->
  case r of
    Cons (Query map) ps ->
      case M.lookup key map of
        Nothing -> Nothing
        Just s -> Just $ Tuple (Cons (Query <<< M.delete key $ map) ps) s
    _ ->  Nothing

params :: Match (M.Map String String)
params = Match $ \r ->
  case r of
    Cons (Query map) ps -> Just $ Tuple ps map
    _ -> Nothing

any :: Match Unit
any = Match $ \r ->
  case r of
    Cons p ps -> Just $ Tuple ps unit
    _ -> Nothing

instance matchFunctor :: Functor Match where
  map f (Match r2t) = Match $ \r ->
    maybe Nothing (\t -> Just $ Tuple (fst t) (f (snd t))) $ r2t r

instance matchAlt :: Alt Match where
  alt (Match a) (Match b) = Match $ \r -> do
    (a r) <|> (b r)

instance matchApply :: Apply Match where
  apply (Match r2a2b) (Match r2a) = Match $ \r1 ->
    case (r2a2b r1) of
      Nothing -> Nothing
      Just (Tuple r2 f) -> case (r2a r2) of
        Nothing -> Nothing
        Just (Tuple r3 b) -> Just $ Tuple r3 (f b)

instance matchPlus :: Plus Match where
  empty = Match \r -> Nothing

instance matchApplicative :: Applicative Match where
  pure a = Match \r -> pure $ Tuple r a

routeFromUrl :: String -> Route
routeFromUrl url | url == "/" = Nil
                 | otherwise = map parsePart $ drop 1 $ toList (S.split "/" url)

parsePart :: String -> RoutePart
parsePart s = fromMaybe (Path s) do
  guard $ S.take 1 s == "?"
  map (Query <<< M.fromList) $ traverse part2tuple parts
  where
  parts :: List String
  parts = toList $ S.split "&" $ S.drop 1 s

  part2tuple :: String -> Maybe (Tuple String String)
  part2tuple part = do
    let param = S.split "=" part
    guard $ A.length param == 2
    Tuple <$> (A.head param) <*> (param A.!! 1)

router :: forall a. String -> Match a -> Maybe a
router url (Match match) = maybe Nothing (Just <<< snd) result
  where result = match $ routeFromUrl url
