module Handler.Language where

import Import

getLanguageR :: Text -> Handler Html
getLanguageR lang = do
  setLanguage lang
  redirectUltDest HomeR
