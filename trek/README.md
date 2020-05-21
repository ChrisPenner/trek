# Trek

Provides a generic monadic interface for diving through and updating arbitrary structures.

It takes inspiration from:

* [jq](https://stedolan.github.io/jq/manual/)
* [meander](https://github.com/noprompt/meander)
* [XQuery](https://en.wikipedia.org/wiki/XQuery)

## Elevator Pitch

Trek allows you to dive down into nested structures while keeping handles on relevant bits as you go.

You can then transform the structure, collect results, etc.

Trek implicitly handles the idea of missing or multiple values for you, meaning you can write your collections or transformations declaratively.
