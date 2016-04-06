module Pux.Router
  ( Match()
  , RoutePart(..)
  , sampleUrl
  , router
  , link
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

import Prelude (class Applicative, class Apply, class Functor, Unit, (<<<), ($), map, (==), bind, (<*>), (<$>), otherwise, pure, unit, (++))

import Control.Monad.Eff (Eff())
import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus)
import Control.MonadPlus (guard)
import Data.Function (runFn3)
import Data.Maybe (Maybe(Just, Nothing), maybe, fromMaybe)
import Data.String as S
import Data.Traversable (traverse)
import Data.Int (fromString)
import Data.Array as A
import Data.List (List(Nil, Cons), toList, drop)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Map as M
import DOM (DOM())
import Pux.Html (Html, Attribute, element)
import Pux.Html.Attributes (attr)
import Global (readFloat, isNaN)
import Signal (constant, Signal)

foreign import createUrlSignal :: forall eff url.
                                  (url -> Signal url) ->
                                  Eff (dom :: DOM | eff) (Signal String)

foreign import linkHandler :: forall a. String -> Attribute a

-- | Returns a signal containing the current window location path and query.
sampleUrl :: forall eff. Eff (dom :: DOM | eff) (Signal String)
sampleUrl = createUrlSignal constant

-- | Creates an anchor that pushes new location to HTML5 history.
-- |
-- | ```purescript
-- | link "/" [] [ text "Home" ]
-- | ```
link :: forall a. String -> Array (Attribute a) -> Array (Html a) -> Html a
link url attrs children = runFn3 element "a" newAttrs children
  where
    newAttrs = attrs ++
      [ linkHandler url
      , attr "href" url
      ]

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
    let param' = S.split "=" part
    guard $ A.length param' == 2
    Tuple <$> (A.head param') <*> (param' A.!! 1)

router :: forall a. String -> Match a -> Maybe a
router url (Match match) = maybe Nothing (Just <<< snd) result
  where result = match $ routeFromUrl url
