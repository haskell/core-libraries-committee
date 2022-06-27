# What

Back in 2019, the CLC [approved](https://gitlab.haskell.org/ghc/ghc/-/issues/20025#note_363426) the monomorphisation of `Data.List`.

At the moment, many functions from [`Data.List`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html) are actually polymorphic by container, allowing any `Foldable f`. The approved proposal was to monomorphise such functions to work on lists only, essentially returning to [`GHC.OldList`](https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-OldList.html) signatures, predating the [Foldable-Traversable Proposal](https://wiki.haskell.org/Foldable_Traversable_In_Prelude). Note that this change does not affect `Prelude`, which will continue to export entities polymorphic by container.

# Why

[The justification](https://gitlab.haskell.org/ghc/ghc/-/issues/20025#note_365364) for the change was:

* A seemingly natural expectation of `Data.List` is that its contents will be monomorphic to lists. This will make it match that expectation.
* If a polymorphic variant is needed, it is better to use `Data.Foldable`. This is in line with the above expectation.
* Uniformity with other container modules (e. g., `Data.Set`, `Data.Map`).
* This makes Haskell easier to learn. Haskell learners typically start with lists, having to understand type classes / `Foldable` before then doesn't make any sense, is unnecessary noise to the learner, and is confusing during reading of the documentation and in error messages.
* Generalized methods rely on inlining for dictionary elimination. Monomorphic ones do not.
* Monomorphisation allows for an easier expansion of the module.

# When

The change was approved without a specific migration plan. The current CLC [developed](https://github.com/haskell/core-libraries-committee/issues/22#issuecomment-1167711639) the following approach:

1. Wait until GHC X.Y makes [`-Wcompat-unqualified-imports`](http://downloads.haskell.org/ghc/9.2.2/docs/html/users_guide/using-warnings.html?highlight=wcompat#ghc-flag--Wcompat-unqualified-imports) a part of `-Wall`, either directly or by including all of `-Wcompat` into `-Wall`. This is tracked [here](https://gitlab.haskell.org/ghc/ghc/-/issues/21791).
  Fixing `-Wcompat-unqualified-imports` should eliminate the majority of breakage, which is caused by conflicting identifiers from `Prelude` (polymorphic) and from `Data.List` (monomorphic). The only remaining cases would be when users explicitly imported `Data.List as L`, but used `L.foldl` for something else, which is arguably wrong.
2. Wait until Stackage for GHC X.Y is available, which roughly coincides with GHC X.Y+1 released.
3. Ask proponents of the change to prepare patches for all Stackage packages. This is likely to be a huge task to perform manually, so volunteers are encouraged to develop a semiautomatic migration tool (in form of `retrie`, regexp, HLS plugin, or whatever technology is available in the glorious future), covering the majority of cases.
4. Once a patch set is ready (it does not need to be merged, just readily available for public), we pull the trigger and make the change in `base` for GHC HEAD (to become X.Y+2 or X.Y+3).

This plan puts the burden on the proponents of the change. If they wish to speed up the process, they can potentially start patching existing packages right now, not even waiting for GHC X.Y. On the other hand, if few people are interested to put their efforts into it, the change is likely to be delayed until a day when the majority of packages choose to comply with `-Wcompat-unqualified-imports` themselves.

# How

If you wish to comply with the future change, please enable `-Wcompat` and fix warnings. A further guidance will be published when we reach Step 4 of the plan above.
