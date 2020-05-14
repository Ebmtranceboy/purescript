module Main where
 
import Prelude
 
import Euler010 (euler10)
import Effect (Effect)
import Effect.Console (log)
 
main :: Effect Unit
main = do
  log $ show $ 17 == euler10 10
  log $ show $ 77 == euler10 20
  log $ show $ 4227 == euler10 200
  log $ show $ 277050 == euler10 2000
  log $ show $ 21171191 == euler10 20000
   --   Assert.equal (fromString "142913828922") (Just $ euler10 2000000)