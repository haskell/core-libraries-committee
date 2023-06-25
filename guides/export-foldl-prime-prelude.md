# What

CLC has approved the
[proposal to export `foldl'` from `class Foldable` from `Prelude`](https://github.com/haskell/core-libraries-committee/issues/167).

Before:

```haskell
module Prelude (
    ...
    Foldable(elem, foldMap, foldr, foldl, foldr1, foldl1, maximum, minimum, product, sum),
    ...
```

After:
```haskell
module Prelude (
    ...
    Foldable(elem, foldMap, foldr, foldl, foldl', foldr1, foldl1, maximum, minimum, product, sum),
    ...
```

# When

The change is likely to happen in GHC 9.8 / `base-4.19`.

# How

While adding new entities is not a breaking change according to PVP, some packages might be broken, because:
* people can and have defined their own `foldl'`s, causing ambiguity errors,
* it can cause new redundant import warnings

However, the breakage is of limited scope,
see [an impact analysis](https://github.com/haskell/core-libraries-committee/issues/167#issuecomment-1579561787) and linked pull requests covering all Stackage packages.

The migration strategy for this change is backward-compatible: you can migrate
already and still retain compatibility with existing GHCs. Because of this,
CLC suggests applying patches at your earliest convenience.

0. To avoid ambiguity errors with your own versions of `foldl'`, we recommend qualifying the usages of your version.

1. To avoid redundant and dodgy import warnings, please hide `Foldable(..)` from `Prelude`.

    For example, if your code used to import `foldl'` from `Data.Foldable` or `Data.List` explicitly, please
    change

    ```haskell
    import Data.Foldable (foldl')
    ```

    to

    ```haskell
    import Prelude hiding (Foldable(..))
    import Data.Foldable (Foldable(..))
    ```

2. There are niche cases where adding more CPP might be required to avoid warnings:
    `foldl'` was not a part of `Foldable` before `base-4.6`, and also
    `Prelude` prior to `base-4.8` exported monomorphic list folds, not generalized
    to `Foldable`. If such extremely wide compatibility range is required, the best option
    is to list all import from `Prelude` explicitly.

    For an example of this see https://github.com/ekmett/hybrid-vectors/pull/10.

# PR template

Here is a template, which you can use when raising PRs against affected
libraries.

> Title: Prepare for foldl' being exported from Prelude
>
> CLC has approved the proposal to export `foldl'` from `Prelude`
> (https://github.com/haskell/core-libraries-committee/issues/167).
>
> The implementation of the proposal is delayed at least to GHC 9.8,
> but one can already future-proof code to be
> compliant with this change in a backwards-compatible way. No CPP required.
>
> Migration guide and more info:
> https://github.com/haskell/core-libraries-committee/blob/main/guides/export-foldl-prime-prelude.md
