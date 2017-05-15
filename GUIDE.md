# The purescript-fetch Guide

The purescript-fetch library facilitates three methods for making data fetching
more efficient:

1. Deduplication of fetches: fetching the resource twice will actually only
   fetch it once.
2. Batching of fetches: fetches that are performed together will be merged into
   a single fetch.
3. Caching of responses: longer-term reduction of fetches of the same resource.

## Concepts

purescript-fetch introduces three concepts that make these possible. Each will
be discussed briefly.

### Keys

Keys can be thought of as primary keys in a database: they uniquely identify
part of a resource. Keys are used to route responses back to the actions that
requested them. Keys are also used as keys in caches, and duplicate keys are
eliminated to implement deduplication.

### Resources

To purescript-fetch, a &ldquo;resource&rdquo; is a law-abiding instance of the
`Resource` type class.

```purescript
-- | A resource implements the fetching of data.
-- |
-- | Instances must satisfy the following laws in addition to the `Ord` laws:
-- |
-- | - Key preservation: `(true <$ _) = map (\m -> all (Map.member <@> m) ks) (resource ks)`
class Ord k <= Resource k r f | k r -> f where
  resource :: Set k -> f (Map k r)
```

In short: `resource` receives all the keys to fetch, fetches them, and returns
a result for each key. It must not omit any of the keys in the result.

Caching is also implemented as a resource, by delegating to another resource
for parts that are not yet or no longer cached.

### Fetches

The `Fetch` data type is used to combine fetches so that they can be
deduplicated and fetched. It has an `Applicative` instance that implements
this. Two functions are of interest here:

```purescript
-- | A computation that fetches data for some key.
fetch :: ∀ k r. Ord k => k -> Fetch k r r

-- | Perform a fetch computation with some resource in some context.
runFetch :: ∀ k r f a. Resource k r f => Functor f => Fetch k r a -> f a
```
