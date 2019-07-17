module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(Nothing))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Alert (alert)
import Effect.Console (log, logShow)
import Effect.Storage (getItem, setItem)
import Foreign (readNullOrUndefined, readString, renderForeignError)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)

newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String
  }

derive instance genericFormData :: Generic FormData _

instance decodeFormData :: Decode FormData where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

instance encodeFormData :: Encode FormData where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

instance showFormData :: Show FormData where
  show = genericShow

loadSavedData :: Effect (Maybe FormData)
loadSavedData = do 
  item <- getItem "person"

  let
      savedData = runExcept do
         json <- traverse readString =<< readNullOrUndefined item
         traverse decodeJSON json

  case savedData of
       Left err -> do
          alert $ "Unable to read saved form data: " <> foldMap (("\n" <> _) <<< renderForeignError) err
          pure Nothing
       Right mdata -> pure mdata

main :: Effect Unit
main = void do
  let rec = FormData { firstName: "Bruce"
                     , lastName: "Wayne"
                     }

  log "Storing data into local storage"
  setItem "person" $ encodeJSON rec
  log "Loading data from local storage"
  loadedRec <- loadSavedData
  traverse logShow loadedRec
