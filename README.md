# uniter

[![CircleCI](https://circleci.com/gh/ejconlon/uniter/tree/master.svg?style=svg)](https://circleci.com/gh/ejconlon/uniter/tree/master)

"I'm a uniter, not a divider!"

This is a generic unification library similar to [unification-fd](https://hackage.haskell.org/package/unification-fd).
Like `ufd` it's generic in the structure being unified, but unlike `ufd` there's no fussing with "freshness"
as everything is chucked into a big "union-map" structure. Currently there is no occurs check either.

The best example is [here](https://github.com/ejconlon/uniter/blob/master/src/Uniter/Example.hs) for now.

Beware: there are probably many bugs here.
