# A _type-level_ symbol spellchecker! 

This almost certainly has no real world use, but was a really good way to become familiar with GHC type checker plugins.

Instances of a `Spellcheck` (in `GHC.TypeLits.Dictionary`) typeclass are generated during type checking _iff_ the symbol is a valid word in the dictionary.

I would like to thank Christiaan Baaij for his excellent [blog post](http://christiaanb.github.io/posts/type-checker-plugin/) and [source code](https://github.com/christiaanb/ghc-typelits-gcd)
on type checker plugins, which I used as a starting point.

_Disclaimer_: I'm not actually sure if the evidence generated is actually the correct way to generate type class instances -- but it seems to work! (The API seems to have changed in GHC >= 8.6 to allow inserting Core, which is used by the [ghc-justdoit](https://github.com/nomeata/ghc-justdoit) plugin.)
