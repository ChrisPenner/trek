  .foodsByName as $foods
| .users
| map( { name: .name
        , favoriteFood: { food: .favoriteFood 
                        , category: $foods[.favoriteFood].category
                        }
        })
