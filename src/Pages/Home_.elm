module Pages.Home_ exposing (view)

import Element
import Html
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , attributes = []
        , element = Element.el [] (Element.text "Home")
    }
