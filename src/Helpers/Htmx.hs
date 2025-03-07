{-# LANGUAGE OverloadedStrings #-}

module Helpers.Htmx where

import Lucid 
import Lucid.Base
import Data.Text (Text)

-- HTMX Attributess
hxPost_ :: Text -> Attribute
hxPost_ = makeAttribute "hx-post"

hxGet_ :: Text -> Attribute
hxGet_ = makeAttribute "hx-get"

hxPut_ :: Text -> Attribute
hxPut_ = makeAttribute "hx-put"

hxDelete_ :: Text -> Attribute
hxDelete_ = makeAttribute "hx-delete"

hxTarget_ :: Text -> Attribute
hxTarget_ = makeAttribute "hx-target"

hxSwap_ :: Text -> Attribute
hxSwap_ = makeAttribute "hx-swap"

hxTrigger_ :: Text -> Attribute
hxTrigger_ = makeAttribute "hx-trigger"

hxIndicator_ :: Text -> Attribute
hxIndicator_ = makeAttribute "hx-indicator"

hxEncoding_ :: Text -> Attribute
hxEncoding_ = makeAttribute "hx-encoding"

hxHeaders_ :: Text -> Attribute
hxHeaders_ = makeAttribute "hx-headers"
