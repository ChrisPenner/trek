module Lib where




-- example = do
--     foods <- use foodsByName
--     collectList $ do
--         user <- flatten users
--         with user $ do
--         result <- sequence' . M.fromList
--                    $ [ ("name": use name)
--                      , ("category": do
--                          userFood <- use favoriteFood
--                          with foods
--                          use (ix userFood . category)
--                        )]



-- {
--   "users": [
--     {
--       "name": "Bob",
--       "favoriteFood": "apple"
--     }
--   ],
--   "foodsByName": {
--     "apple": {
--       "category": "fruit"
--     }
--   }
-- }
--
--   .foodsByName as $foods
-- | .users
-- | map( { name: .name
--         , favoriteFood: { food: .favoriteFood
--                         , category: $foods[.favoriteFood].category
--                         }
--         })
