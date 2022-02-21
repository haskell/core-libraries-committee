# What

CLC has approved the [proposal to remove the `ST` instance of the `MonadFail` class 
from `base`][proposal]. `MonadFail` consists of the single method `fail`, which is
meant to be implemented in terms of the monad itself. In contrast, the `ST` instance
currently (GHC 9.2, `base-4.16.0.0`) have:

```haskell
    fail s = errorWithoutStackTrace s
```

This goes against the above-mentioned idea behind the `fail` method. 

[proposal]: https://github.com/haskell/core-libraries-committee/issues/33

# When

At the very earliest the change may appear in GHC 9.4 (~ Q3 2022), but
given that a feature freeze for 9.4 is coming soon, it's more likely to
happen in GHC 9.6 (~ Q2 2023).

# How

This is a breaking change, but of limited scope. An impact analysis showed 
that only 11 packages of current Stackage subset require updates, and all patches
are presented here: https://github.com/hs-monadfail-st-remove or have already been
implemented by the maintainers.

The migration strategy for this change is backward-compatible: you can migrate
already and still retain compatibility with existing GHCs. Because of this
CLC suggests applying patches at your earliest convenience.

You are affected if you use the following inside `ST` monad.

1. If you use `fail`, replace it with `error`.

2. If you use a failable pattern, e.g.:

```haskell
  (x:xs) <- someFunc
```

you can replace it with a combination of `bind` and `let`, e.g.:

```haskell
  xs' <- someFunc
  let (x:xs) = xs'
```

Finally, if you use `ST` to instantiate a class that requires `MonadFail` from the
respective parameter, that is a problem with no obvious solution. We found only one
example of this on Stackage, and in that case it was trivial to change the class so that
it does not require `MonadFail` anymore.

# PR template

Here is a template, which you can use when raising PRs against affected
libraries.

> Title: Prepare for MonadFail/ST instance removal from base 
>
> CLC has approved the proposal to remove the `ST` instance of `MonadFail` from `base`
> (https://github.com/haskell/core-libraries-committee/issues/33).
>
> The implementation of the proposal is delayed at least to GHC 9.4 and likely
> to GHC 9.6, but one can already future-proof code to be
> compliant with this change in a backwards-compatible way. No CPP required.
>
> Migration guide and more info:
> https://github.com/haskell/core-libraries-committee/blob/main/guides/no-monadfail-st-inst.md

