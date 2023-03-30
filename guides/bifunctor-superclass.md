# What
The CLC has approved the [proposal #91](https://github.com/haskell/core-libraries-committee/issues/91), to make Functor a quantified superclass of Bifunctor.
A law now requires that `second = fmap`.

Before:
```haskell
class Bifunctor p where
```

After:
```haskell
class (forall a. Functor (p a)) => Bifunctor p where
```

# When

The change has happened in GHC 9.6.

# How

This is a breaking change:

- People can no longer define `Bifunctor` without `Functor`.

However, the breakage is of limited scope.
An impact analysis (please see [comments following here](https://github.com/haskell/core-libraries-committee/issues/91#issuecomment-1278299233))
showed that only a few of the packages that sucessfully pass dependency resolution on Stackage `nightly-2022-06-17` break with this change.
Patches have been submitted to each of them.

The migration policy for this change is backwards-compatible: you can migrate already and still retain compatibility with existing GHCs.
Because of this, CLC suggests applying patches at your earliest convenience.

### Add any missing Functor instances.

If you defined instance `Bifunctor Foo` but omitted instance `Functor (Foo a)`, you must now define the latter.

You can do this by adding the language pragma `DeriveFunctor` and declaring `derive Functor` at the data type definition.
Alternatively you can explicitely define `fmap`.

In the context of this migration we recommend to define `fmap` canonically, because there could be a `noncanonical-bifunctor-instances` warning in the future. Canonical instances define the subclass in terms of the superclass but not vice versa. Accordingly the definitions `fmap = second` and `fmap = bimap id` are noncanonical. We recommend to avoid them and move any definition of `second` to `fmap` to instead define `second = fmap`:
   ```diff
   + {-# LANGUAGE DeriveFunctor #-}
   
     data MyType a b = MyLeft a | MyRight b
   -   deriving (Show, Eq)
   +   deriving (Show, Eq, Functor)

     instance Bifunctor MyType where
       first f _ (MyLeft a) = MyLeft (f a)
       first _ _ x          = x
       
   -   second _ f (MyRight b) = MyRight (f b)
   -   second _ _ x          = x
   +   second = fmap
   ```

   Example changes:

   - [Add `Functor (TreeNode v)` instance in hgeometry-combinatorial](https://gitlab.haskell.org/ghc/head.hackage/-/blob/8fa2a74d736914a28d1fefbba21cd5325eefe63a/patches/hgeometry-combinatorial-0.14.patch#L140)

   - [Add `Functor (These a)` instance in aura](https://gitlab.haskell.org/ghc/head.hackage/-/blob/8fa2a74d736914a28d1fefbba21cd5325eefe63a/patches/aura-3.2.9.patch#L44)

   - [Add `Functor (SPForest r)` instance in ForestStructures moving the definition of second](https://github.com/choener/ForestStructures/pull/2/files)

# PR template

Here is a template, which you can use when raising PRs against affected libraries.

> Title: Add Functor instance for _
>
> CLC has approved the proposal to make Functor a quantified superclass of Bifunctor
> (https://github.com/haskell/core-libraries-committee/issues/91#).
> `forall a. Functor (f a)` is now a superclass of `Bifunctor f`.
>
> This means that you can not longer write a `Bifunctor` instance without a
> corresponding `Functor` instance.
>
> The implementation of the proposal is slated for GHC 9.6, but one can
> already future-proof code to be compliant with this change in a
> backwards-compatible way. No CPP required.
>
> Migration guide and more info:
> https://github.com/haskell/core-libraries-committee/blob/main/guides/bifunctor-superclass.md
