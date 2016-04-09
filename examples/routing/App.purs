module RoutingExample.App where

import Prelude ((++), show)
import Pux.Html (Html, div, nav, ul, li, h1, text)
import Pux.Router (link)

import RoutingExample.Routes (Route(Home, Users, User, NotFound))

data Action = PageView Route

type State = { currentRoute :: Route }

init :: State
init = { currentRoute: Home }

update :: Action -> State -> State
update (PageView route) state = state { currentRoute = route }

view :: State -> Html Action
view state =
  div [] [ navigation, page state.currentRoute ]

page :: Route -> Html Action
page Home      = h1 [] [ text "Home" ]
page Users     = h1 [] [ text "Users" ]
page (User id) = h1 [] [ text ("User: " ++ show id) ]
page NotFound  = h1 [] [ text "Not Found" ]

navigation :: Html Action
navigation =
  nav
    []
    [ ul
      []
      [ li [] [ link "/" [] [ text "Home" ] ]
      , li [] [ link "/users" [] [ text "Users" ] ]
      , li [] [ link "/users/123" [] [ text "User 123" ] ]
      , li [] [ link "/foobar" [] [ text "Not found" ] ]
      ]
    ]
