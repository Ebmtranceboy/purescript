I want
      {
        "c": [
          {
            "v": ["998"],
            "d": "998"
          }
        ],
        "s": false
      }
to become
{ v: "998", d: "998 }

parseC json = do
  obj <- decodeJson json
  c <- obj .: "c"
  c' <- note "No c value" $ Array.head c
  v <- c' .: "v"
  v' <- note "No v value" $ Array.head v
  d <- c' .: "d"
  pure { v: v', d }
  
cf
newtype Character
  = Character { height :: Int, name :: String, films :: Array String }

 decodeJson
      >=> \all -> ado
          height <-
            Int.fromString all.height
              # Either.note "no no"
          in Character $ all { height = height } 
