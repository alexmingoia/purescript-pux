module Pux.Router
  ( Match(..)
  , Route
  , RoutePart(..)
  , router
  , lit
  , str
  , num
  , int
  , bool
  , parseSegment
  , param
  , params
  , any
  , end
  ) where

import Control.Alt (class Alt)
import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply, (<*>))
import Control.Bind (discard)
import Control.MonadPlus (guard)
import Control.Plus (class Plus)
import Data.Array as A
import Data.Boolean (otherwise)
import Data.Eq ((==))
import Data.Functor (class Functor, map, (<$>))
import Data.Function (($), (<<<))
import Data.Foldable (foldr)
import Data.Int (fromString)
import Data.List (catMaybes, List(Nil, Cons), fromFoldable, drop)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Map as M
import Data.Profunctor (lmap)
import Data.String as S
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Unit (Unit, unit)
import Global (readFloat, isNaN)

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
lit part = parseSegment parse
  where
    parse s
      | s == part = Just unit
      | otherwise = Nothing

parseSegment :: forall a. (String -> Maybe a) -> Match a
parseSegment parser = Match $ \r ->
  case r of
    Cons (Path p) ps -> map (Tuple ps) $ parser p
    _ -> Nothing

num :: Match Number
num = parseSegment parse
  where
    parse p = let res = readFloat p in
      if isNaN res then
        Nothing
      else
        Just res

int :: Match Int
int = parseSegment fromString

bool :: Match Boolean
bool = parseSegment parse
  where
    parse "true" = Just true
    parse "false" = Just false
    parse _ = Nothing

str :: Match String
str = parseSegment Just

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
  alt (Match a) (Match b) = Match $ \r ->
    -- Manual implementation to avoid unnecessary evaluation of b r in case a r is true.
    case a r of
      Nothing -> b r
      Just x  -> Just x

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
routeFromUrl "/" = Nil
routeFromUrl url = case S.indexOf (S.Pattern "?") url of
                    Nothing -> parsePath Nil url
                    Just queryPos ->
                      let queryPart = parseQuery <<< S.drop queryPos $ url
                      in parsePath (Cons queryPart Nil) <<< S.take queryPos $ url
  where
    parsePath :: Route -> String -> Route
    parsePath query = drop 1 <<< foldr prependPath query <<< S.split (S.Pattern "/")
      where prependPath = lmap Path Cons

parseQuery :: String -> RoutePart
parseQuery s = Query <<< M.fromFoldable <<< catMaybes <<< map part2tuple $ parts
  where
  parts :: List String
  parts = fromFoldable $ S.split (S.Pattern "&") $ S.drop 1 s

  part2tuple :: String -> Maybe (Tuple String String)
  part2tuple part = do
    let param' = S.split (S.Pattern "=") part
    guard $ A.length param' == 2
    Tuple <$> (A.head param') <*> (param' A.!! 1)

router :: ∀ a. String -> Match a -> Maybe a
router url (Match match) = maybe Nothing (Just <<< snd) result
  where result = match $ routeFromUrl url
