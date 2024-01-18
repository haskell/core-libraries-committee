# What

CLC has approved the proposal to
[add `{-# WARNING #-}` to `Data.List.{head,tail}`](https://github.com/haskell/core-libraries-committee/issues/87). This means that the existing users of these functions will receive a compile-time warning about their partiality.

# When

The change is scheduled for GHC 9.8.

# How

This is not a breaking change, unless a package forces `-Werror`.

If you don't want to change any Haskell code and want to carry on with using `Data.List.{head,tail}`:

1. If you use `-Werror`, set `-Wwarn=x-partial` to downgrade this area of compiler messages from errors to warnings, so that your build can succeed. Note that `x-partial` is a new group of warnings, introduced in GHC 9.8, so you might need to additionally set `-Wno-unrecognised-warning-flags` or `-Wwarn=unrecognised-warning-flags` to maintain backward compatibility with older GHC releases.

2. If you are annoyed by warnings, set `-Wno-x-partial` to quash this area of compiler messages. Use `-Wno-unrecognised-warning-flags` to maintain backward compatibility with older GHC releases. To disable the warnings once and forever in GHCi, put `:set -Wno-x-partial -Wno-unrecognised-warning-flags` into `~/.ghci` config file.
   Beware that this also disables any other warnings in the custom `-x-partial` warning group!

If you are happy to change code to eliminate warning messages:

1. The easiest way to migrate away from `tail` is to replace it with `drop 1`. This is a drop-in replacement unless you are relying on `tail`'s partiality.

2. Instead of `head` you can use explicit pattern matching: replace any `head foo` with

    ```haskell
    case foo of
      [] -> error "<explain, why it is impossible to encounter an empty list here>"
      hd : _ -> hd
    ```

3. Another option is to use [`Data.List.uncons`](https://hackage.haskell.org/package/base/docs/Data-List.html#v:uncons) or [`Data.Maybe.listToMaybe`](https://hackage.haskell.org/package/base/docs/Data-Maybe.html#v:listToMaybe): replace any `head foo` with

    ```haskell
    maybe (error "...") fst (uncons foo)
    ```

    or

    ```haskell
    fromMaybe (error "...") (listToMaybe foo)
    ```

4. It is even better to enforce non-emptiness of the list in types, using [`Data.List.NonEmpty`](https://hackage.haskell.org/package/base/docs/Data-List-NonEmpty.html). This might however require non-local refactoring.

5. If the list is guaranteed to be not only non-empty, but actually infinite, one can be benefit from enforcing this invariant in types as well. E. g., use  [`Stream`](https://hackage.haskell.org/package/Stream/docs/Data-Stream.html), [`streams`](https://hackage.haskell.org/package/streams/docs/Data-Stream-Infinite.html) or [`infinite-list`](https://hackage.haskell.org/package/infinite-list/docs/Data-List-Infinite.html).

6. As an ultimate measure you can hide `Prelude.{head,tail}` and define your own in a utility module, or use `headErr` and `tailErr` from [`Safe`](https://hackage.haskell.org/package/safe-0.3.20/docs/Safe.html#g:2).

# PR template

Here is a template, which you can use when raising PRs against affected libraries.

> Title: Avoid `Data.List.{head,tail}`
>
> CLC has approved the proposal to add `{-# WARNING #-}` to `Data.List.{head,tail}`
> (https://github.com/haskell/core-libraries-committee/issues/87).
> It means that usage of `head` and `tail` will emit compile-time warnings
> starting from GHC 9.8.
>
> Migration guide and more info:
> https://github.com/haskell/core-libraries-committee/blob/main/guides/warning-for-head-and-tail.md
