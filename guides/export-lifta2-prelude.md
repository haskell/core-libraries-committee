# What

CLC has approved the
[proposal to export `liftA2` from `class Applicative` from `Prelude`](https://github.com/haskell/core-libraries-committee/issues/3).
This effectively means that the entirety of the class will now be exported from `Prelude`.

Before:

```haskell
module Prelude (
    ...
    Applicative(pure, (<*>), (*>), (<*)),
    ...
```

After:
```haskell
module Prelude (
    ...
    Applicative(pure, (<*>), (*>), (<*), liftA2),
    ...
```

# When

At the very earliest, the change may appear in GHC 9.6 (~ Q1 2023).

# How

This is a breaking change:
* people can and have defined their own `liftA2`s, causing ambiguity errors
* it can cause new redundant import warnings

However, the breakage is of limited scope.
An impact analysis (please see [comments following here](https://github.com/haskell/core-libraries-committee/issues/50#issuecomment-1141704595))
showed that none of the packages that sucessfully pass dependency resolution on Stackage `nightly-2022-03-20` break with this change.

With a newer stackage nightly, `pandoc` breaks, and a patch has already been [submitted][pandoc-patch].

The migration policy for this change is backward-compatible: you can migrate
already and still retain compatibility with existing GHCs. Because of this,
CLC suggests applying patches at your earliest convenience.

You can also refer to the following submitted patches for examples:

* [`Cabal-syntax`](https://github.com/haskell/cabal/pull/8223)
* [`containers`](https://github.com/haskell/containers/pull/841)
* [`pandoc`][pandoc-patch]

0. To avoid ambiguity errors with your own versions of `liftA2`, we recommend qualifying the usages of your version.

1. To avoid redundant and dodgy import warnings, please hide `Applicative(..)` from `Prelude`.

    For example, if your code used to import `liftA2` from `Control.Applicative` explicitly, please
    change

    ```haskell
    import Control.Applicative (liftA2)
    ```

    to

    ```haskell
    import Prelude hiding (Applicative(..))
    import Control.Applicative (Applicative(..))
    ```

    Similarly, if your code used to import `Applicative(..)` from `Control.Applicative`, please
    add

    ```haskell
    import Prelude hiding (Applicative(..))
    ```

2. There are niche cases where adding more CPP might be required to avoid warnings.

    Due to the fact that `liftA2` wasn't always in the `Applicative` class,
    codebases that aim to support older `base` versions (`<4.10`) will not be able to rely on
    hiding `Applicative` from `Prelude` and instead importing it from `Control.Applicative` to get `liftA2`.
    For an example of this, see https://github.com/haskell/cabal/pull/8223.

[pandoc-patch]: https://github.com/jgm/pandoc/pull/8132
# PR template

Here is a template, which you can use when raising PRs against affected
libraries.

> Title: Prepare for liftA2 being exported from Prelude
>
> CLC has approved the proposal to export `liftA2` from `Prelude`
> (https://github.com/haskell/core-libraries-committee/issues/50).
> It means that the entirety of `Applicative` will now be exported from `Prelude`.
>
> The implementation of the proposal is delayed at least to GHC 9.6,
> but one can already future-proof code to be
> compliant with this change in a backwards-compatible way. No CPP required.
>
> Migration guide and more info:
> https://github.com/haskell/core-libraries-committee/blob/main/guides/export-lifta2-prelude.md
