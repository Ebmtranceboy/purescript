module Main where

import Prelude hiding (div)

import Affjax (get) as Affjax
import Affjax.ResponseFormat (json) as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (toArray, toObject, toString) as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe, fromJust)
import Data.Traversable (traverse)
import Data.String.NonEmpty as NonEmpty
import Debug.Trace (spy)
import Effect (Effect, foreachE)
import Effect.Aff (Aff, error, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Console (error, log) as Console
import Foreign.Object (lookup) as Object
import Web.DOM (Document) as DOM
import Web.DOM.Element (Element, setAttribute, setId, toEventTarget, toNode) as DOM
import Web.DOM.Node (appendChild, parentNode, removeChild, setTextContent) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.Event.Event (Event) as Event
import Web.Event.EventTarget (addEventListener, eventListener) as Event
import Web.HTML.Event.EventTypes (click) as Event
import Web.HTML (window) as HTML
import Web.HTML.Window (document) as HTML
import Web.HTML.HTMLDocument (body, toDocument) as HTML
import Web.DOM.Document (createElement, toNonElementParentNode) as DOM
import Web.HTML.HTMLElement (toNode) as HTML
import Web.HTML.HTMLInputElement (fromElement, value) as HTMLInput
import Partial.Unsafe (unsafePartial)

type RedditPost = { title :: String, selftext :: Maybe String, id :: String }

textInput :: DOM.Document -> Effect DOM.Element
textInput document = do
  element     <- DOM.createElement "input" document 
  DOM.setAttribute "type" "text" element
  pure element 
 
label :: String -> DOM.Document -> Effect DOM.Element
label text document = do
  elem <- DOM.createElement "label" document
  DOM.setTextContent text (DOM.toNode elem)
  pure elem 

button :: String -> (Event.Event -> Effect Unit) -> DOM.Document -> Effect DOM.Element
button text onClick document = do
  elem      <- DOM.createElement "button" document 
  _         <- DOM.setTextContent text (DOM.toNode elem)
  let target = DOM.toEventTarget elem
  listener  <- Event.eventListener onClick
  _         <- Event.addEventListener Event.click listener false target
  pure elem

section :: DOM.Document -> Effect DOM.Element
section document = DOM.createElement "section" document 

div :: DOM.Document -> Effect DOM.Element
div = DOM.createElement "div"

ul :: DOM.Document -> Effect DOM.Element
ul = DOM.createElement "ul"

li :: DOM.Document -> Effect DOM.Element
li = DOM.createElement "li"

p :: DOM.Document -> Effect DOM.Element
p = DOM.createElement "p"

link :: String -> String -> DOM.Document -> Effect DOM.Element
link href text document = do
  elem <- DOM.createElement "a" document
  DOM.setTextContent text (DOM.toNode elem)
  DOM.setAttribute "href" href elem
  pure elem

post :: RedditPost -> DOM.Document -> Effect DOM.Element
post redditPost document = do
  container <- div document 
  input <- DOM.getElementById "subreddit" (DOM.toNonElementParentNode document)
  value <- traverse HTMLInput.value (HTMLInput.fromElement $ unsafePartial $ fromJust input)
  let subreddit = unsafePartial $ fromJust value
  a <- link ("https://www.reddit.com/r/" <> subreddit <> "/comments/" <> redditPost.id) redditPost.title document
  paragraph <- p document
  let text  = fromMaybe "" redditPost.selftext
  DOM.setTextContent text (DOM.toNode paragraph)
  let containerNode = DOM.toNode container
  DOM.appendChild (DOM.toNode a) containerNode # void
  DOM.appendChild (DOM.toNode paragraph) containerNode # void
  pure container

posts :: Array RedditPost -> DOM.Document -> Effect DOM.Element
posts redditPosts document = do
  list <- ul document
  DOM.setId "posts" list 
  let listNode = DOM.toNode list
  foreachE redditPosts \redditPost -> do
    listItem <- li document
    item <- post redditPost document
    DOM.appendChild (DOM.toNode item) (DOM.toNode listItem) # void
    DOM.appendChild (DOM.toNode listItem) listNode # void
  pure list 

liftEither :: forall a b. String -> Either a b -> Aff b
liftEither errorMessage (Left err) = throwError (error errorMessage)
liftEither _ (Right val) = pure val

liftMaybe :: forall a. String -> Maybe a -> Aff a
liftMaybe errorMessage = maybe (throwError (error errorMessage)) pure 

controls :: DOM.Document -> Effect DOM.Element
controls document = do
  repoLabel   <- label "Subreddit" document
  input       <- textInput document
  _           <- DOM.setId "subreddit" input
  goButton    <- button "Go" onClick document
  container   <- section document
  let containerNode = DOM.toNode container
  DOM.appendChild (DOM.toNode repoLabel) containerNode # void
  DOM.appendChild (DOM.toNode input) containerNode # void
  DOM.appendChild (DOM.toNode goButton) containerNode # void
  pure container
    
    where

      onClick :: Event.Event -> Effect Unit
      onClick event = launchAff_ do
        either <- try doAjax 
        case either of 
          Right _ -> Console.log "Ajax complete!" # liftEffect
          Left error -> liftEffect $ Console.error ("Error performing ajax request to reddit: " <> (show error))
      
        where
          doAjax :: Aff Unit
          doAjax = do
            input         <- DOM.getElementById "subreddit" (DOM.toNonElementParentNode document) 
                              # liftEffect
                              >>= liftMaybe "Couldn't find subreddit text field"
            value         <- traverse HTMLInput.value (HTMLInput.fromElement input) 
                              # liftEffect  >>= liftMaybe "Subreddit Element is not an input field"
            neVal         <- NonEmpty.fromString value # liftMaybe "Subreddit is empty"
            redditPosts   <- fetchPosts $ NonEmpty.toString neVal
            postsSection  <- DOM.getElementById "posts" (DOM.toNonElementParentNode document) 
                              # liftEffect
                              >>= liftMaybe "Couldn't find the 'posts' section"
            let postsSectionNode = DOM.toNode postsSection
            parent        <- DOM.parentNode (DOM.toNode postsSection) 
                              # liftEffect 
                              >>= liftMaybe "Couldn't find the parent of the 'posts' section"
            newPostsElem  <- posts redditPosts document # liftEffect
            DOM.removeChild postsSectionNode parent # liftEffect # void
            DOM.appendChild (DOM.toNode newPostsElem) parent # liftEffect # void

            
      fetchPosts :: String -> Aff (Array RedditPost)
      fetchPosts subreddit = do
        let url = "https://www.reddit.com/r/" <> subreddit <> "/new.json"
        json <- Affjax.get ResponseFormat.json url <#> _.body >>= 
                  liftEither "Request to reddit failed to decode"
        liftMaybe "Couldn't properly decode json" (spy "maybeChildren" $ maybeChildren ({-spy "original"-} json))
     
        where
              
           maybeChildren :: Json -> Maybe (Array RedditPost)
           maybeChildren json = 
             JSON.toObject ({-spy "maybeJson"-} json) >>=
               Object.lookup "data" >>= JSON.toObject >>=
               Object.lookup "children" >>= JSON.toArray >>=
               traverse childToRecord

           childToRecord :: Json -> Maybe RedditPost 
           childToRecord json = do
             obj           <- JSON.toObject ({-spy "json"-} json)
             dataObj       <- Object.lookup "data" obj >>= JSON.toObject
             title         <- Object.lookup "title" dataObj >>= JSON.toString
             let selftext  =  Object.lookup "selftext" dataObj >>= JSON.toString 
             id            <- Object.lookup "id" dataObj >>= JSON.toString
             pure { title, selftext, id }
                
main :: Effect Unit
main = do
  window        <- HTML.window
  htmlDocument  <- HTML.document window
  let document  =  HTML.toDocument htmlDocument
  maybeBody     <- HTML.body htmlDocument
  case maybeBody of 
    Nothing   -> Console.error "no body element found!" 
    Just body -> do
      ctrls <- controls document 
      let bodyNode = HTML.toNode body
      DOM.appendChild (DOM.toNode ctrls) bodyNode # void
      postsList <- posts [] document
      DOM.appendChild (DOM.toNode postsList) bodyNode # void 


