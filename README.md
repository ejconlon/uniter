# uniter

"I'm a uniter, not a divider!"

This is a generic unification library similar to [unification-fd](https://hackage.haskell.org/package/unification-fd).
Like `ufd` it's generic in the structure being unified, but unlike `ufd` there's no fussing with "freshness"
as everything is chucked into a big "union-map" structure.

It depends on [overeasy](https://github.com/ejconlon/overeasy) at HEAD not for the e-graph but for some expression and container code.

Beware: there are many bugs here.
