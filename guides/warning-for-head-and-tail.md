# What

CLC has approved the proposal to
[add `{-# WARNING #-}` to `Data.List.{head,tail}`](https://github.com/haskell/core-libraries-committee/issues/87). This means that the existing users of these functions will receive a compile-time warning about their partiality.

# When

At the very earliest, the change may appear in GHC 9.6 (~ Q1 2023), but more likely GHC 9.8 (~ Q4 2023).

# How

This is not a breaking change, unless a package forces `-Werror`.

If you don't want to change any Haskell code and want to carry on with using `Data.List.{head,tail}`:

1. If you use `-Werror`, set `-Wwarn=warnings-deprecations` to downgrade this area of compiler messages from errors to warnings, so that your build can succeed.

2. If you are annoyed by warnings, set `-Wno-warnings-deprecations` to quash this area of compiler messages. You can also put `:set -Wno-warnings-deprecations` into `.ghci` config file.
   Note that this also disables any other custom warnings and deprecation warnings!

If you are happy to change code to eliminate warning messages:

1. The easiest way to migrate away from `tail` is to replace it with `drop 1`. This is a drop-in replacement unless you are relying on `tail`'s partiality.

2. Instead of `head` you can use explicit pattern matching: replace any `head foo` with

    ```haskell
    case foo of
      [] -> error "<explain, why it is impossible to encounter an empty list here>"
      hd : _ -> hd
    ```

3. Another option is to use [`Data.List.uncons`](https://hackage.haskell.org/package/base/docs/Data-List.html#v:uncons): replace any `head foo` with

    ```haskell
    fromMaybe (error "...") fst (uncons foo)
    ```

4. It is even better to enforce non-emptiness of the list in types, using [`Data.List.NonEmpty`](https://hackage.haskell.org/package/base/docs/Data-List-NonEmpty.html). This might however require non-local refactoring.

5. If the list is guaranteed to be not only non-empty, but actually infinite, one can be benefit from enforcing this invariant in types as well. E. g., use  [`Stream`](https://hackage.haskell.org/package/Stream/docs/Data-Stream.html) or [`streams`](https://hackage.haskell.org/package/streams/docs/Data-Stream-Infinite.html).

6. As an ultimate measure you can hide `Prelude.{head,tail}` and define your own in a utility module.

# Related work

Consider contributing to https://github.com/ghc-proposals/ghc-proposals/pull/454 and/or https://github.com/ghc-proposals/ghc-proposals/pull/541, which expand and make GHC warnings mechanism more flexible, allowing more mitigation strategies for this sort of changes.

# PR template

Here is a template, which you can use when raising PRs against affected libraries.

> Title: Avoid Data.List.{head,tail}
>
> CLC has approved the proposal to add `{-# WARNING #-}` to `Data.List.{head,tail}`
> (https://github.com/haskell/core-libraries-committee/issues/87).
> It means that usage of `head` and `tail` will emit compile-time warnings.
>
> The implementation of the proposal is delayed at least to GHC 9.6,
> but it's advisable to start a gradual migration away already.
>
> Migration guide and more info:
> https://github.com/haskell/core-libraries-committee/blob/main/guides/warning-for-head-and-tail.md
