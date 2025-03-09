{-# LANGUAGE OverloadedStrings #-}

module Helpers.Htmx where

import Lucid 
import Lucid.Base
import Data.Text (Text)

-- HTMX Attributes
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

hxPushUrl_ :: Text -> Attribute
hxPushUrl_ = makeAttribute "hx-push-url"

hxSelect_ :: Text -> Attribute
hxSelect_ = makeAttribute "hx-select"

hxSync_ :: Text -> Attribute
hxSync_ = makeAttribute "hx-sync"

hxValidate_ :: Text -> Attribute
hxValidate_ = makeAttribute "hx-validate"

dataValue_ :: Text -> Attribute
dataValue_ = makeAttribute "data-value"

hxOn_ :: Text -> Text -> Attribute
hxOn_ event handler = makeAttribute ("hx-on:" <> event) handler
