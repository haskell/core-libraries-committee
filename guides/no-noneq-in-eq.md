# What

CLC has approved the
[proposal to remove `(/=)` from `class Eq`](https://github.com/haskell/core-libraries-committee/issues/3).
It means that `class Eq` will contain a single member `(==)` and `(/=)` will
be just a normal function available from `Prelude`.

Before:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

After:

```haskell
class Eq a where
  (==) :: a -> a -> Bool

(/=) :: Eq a => a -> a -> Bool
```

# When

The proposal was approved conditionally, pending a resolution of
[-Winline-rule-shadowing: Different for single and multi method classes?](https://gitlab.haskell.org/ghc/ghc/-/issues/20535)
issue. At the very earliest the change may appear in GHC 9.4 (~ Q3 2022), but
given that a feature freeze for 9.4 is coming soon, it's more likely to
happen in GHC 9.6 (~ Q2 2023).

# How

This is a breaking change, but of limited scope. If you import `Prelude` in
full and either derive `Eq` or define only `(==)` in your instances, there is
nothing to change. An impact analysis showed that only 45 package of current
Stackage subset require updates, and thanks to @phadej we have all patches at
hand: https://gist.github.com/phadej/0e810832685cb4e70e3ca3a61e2f33d1

The migration policy for this change is backward-compatible: you can migrate
already and still retain compatibility with existing GHCs. Because of this
CLC suggests applying patches at your earliest convenience.

1. If your code used to import `Eq(..)` from `Prelude` explicitly, please
change

```haskell
import Prelude (Eq(..))
```

to

```haskell
import Prelude (Eq, (==), (/=))
```

2. If your code used to import `Eq(..)` directly from `Data.Eq`, please
change

```haskell
import Data.Eq (Eq(..))
```

to

```haskell
import Data.Eq (Eq, (==), (/=))
```

3. If your code used to define both members of `Eq`, just remove the
definition of `(/=)`, so that

```haskell
instance Eq Foo where
  x == y = foo x y
  x /= y = bar x y
```

becomes

```haskell
instance Eq Foo where
  x == y = foo x y
```

4. If your code used to define only `(/=)`, implement `(==)` instead, so that

```haskell
instance Eq Foo where
  x /= y = foo x y
```

becomes

```haskell
instance Eq Foo where
  x == y = not (foo x y)
```

That's it!

# PR template

Here is a template, which you can use when raising PRs against affected
libraries.

> Title: Future-proof class Eq
>
> CLC has approved the proposal to remove `(/=)` from `class Eq`
> (https://github.com/haskell/core-libraries-committee/issues/3).
> It means that `class Eq` will contain a single member `(==)` and `(/=)` will
> be just a normal function.
>
> The implementation of the proposal is delayed at least to GHC 9.4 and likely
> to GHC 9.6, but one can already future-proof code to be
> compliant with this change in a backwards-compatible way. No CPP required.
>
> Migration guide and more info:
> https://github.com/haskell/core-libraries-committee/blob/main/guides/no-noneq-in-eq.md
>
