# What
The CLC has approved the [proposal #93](https://github.com/haskell/core-libraries-committee/issues/93), to make Foldable and Traversable quantified superclasses of Bifoldable and Bitraversable respectively.
These superclass constraints also give rise to the following laws:
```
-- Bifoldable
bifoldl const = foldl
bifoldr (flip const) = foldr
bifoldMap (const mempty) = foldMap
-- Bitraversable
bitraverse pure = traverse
```

Before:
```haskell
class Bifoldable p where

class (Bifunctor t, Bifoldable t) => Bitraversable t where
```

After:
```haskell
class (forall a. Foldable (p a)) => Bifoldable p where

class (forall a. Traversable (t a), Bifunctor t, Bifoldable t) => Bitraversable t where
```

### New Laws

Intuitively the new laws constrain the order in which values are traversed/folded. The Bitraversal/Bifold order must respect the Traversal/Fold order: when bitraversing/bifolding a value `p a b` we interleave the visits of `a` values with the traversal/fold order of `b` values given by the Traversable/Foldable instance for `p a`. For example the following definition would be unlawful, because `bifold` and `fold` do not visit `b` values in the same order:
```haskell
newtype BadBifoldable p a b = BadBifoldable (p a b)
    deriving stock (Show, Eq)
    deriving (Functor, Foldable) via (Reverse (p a))
    deriving newtype (Bifunctor, Bifoldable)
```
Pre-order, in-order, post-order and level-order [traversals of trees](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Traversable.html#g:12) are another example of traversals of the same data type in different orders. Such an order difference gives rise to instances that violate the new laws but otherwise satisfy all the previous Bifoldable, Bitraversable, Foldable and Traversable laws.

# When

The change has happened in GHC 9.8.

# How

This is a breaking change:

- People can no longer define `Bifoldable` without `Foldable`.
- Previously lawful `Bifoldable` instances may now violate the new laws that relate `Bifoldable` to `Foldable`.
- People can no longer define `Bitraversable` without `Traversable`.
- Previously lawful `Bitraversable` instances may now violate the new law that relates `Bitraversable` to `Traversable`.

However, the breakage is of limited scope.
An impact analysis (please see [comments following here](https://github.com/haskell/core-libraries-committee/issues/93#issuecomment-1585930550))
showed that only a few of the packages that successfully pass dependency resolution on Stackage `nightly-2022-06-17` fail to compile with this change.
Patches have been submitted to each of them.

The migration policy for this change is backwards-compatible: you can migrate already and still retain compatibility with existing GHCs.
Because of this, CLC suggests applying patches at your earliest convenience.

### Add any missing Foldable instances.

If you defined instance `Bifoldable Foo` but omitted instance `Foldable (Foo a)`, you must now define the latter.

You can do this by adding `{-# LANGUAGE DeriveFoldable #-}` at the top of the file and `deriving Foldable` at the data type definition. You should ensure that the new laws are satisfied.
Alternatively you can explicitly define `foldr` and/or `foldMap`. The following definitions rely on an existing Bifoldable instance and ensure that the new laws are satisfied:
```
class Foldable (Foo a) where
  foldr = bifoldr (flip const)
  foldMap = bifoldMap (const mempty)
```
You can also derive Foldable by relying on a Traversable instance.
```haskell
import Data.Traversable (foldMapDefault)

class Foldable (Foo a) where
  foldMap = foldMapDefault
```
In this case the new laws relating Foldable and Bifoldable are necessarily satisfied if the Traversable and Bifoldable instances were defined via a common Bitraversable instance, i.e. via `traverse = bitraverse pure` and `bifoldMapDefault`.

   Example changes:

   - [Add `Foldable (UnderScope n f e)` instance in blanks defined via DeriveFoldable](https://github.com/ejconlon/blanks/pull/1/files)

   - [Add Foldable instances in futhark defined via foldMapDefault](https://github.com/diku-dk/futhark/pull/1955/files)

   - [Added Foldable instances in hgeometry and hgeometry-combinatorial](https://github.com/noinia/hgeometry/pull/210/files)

### Add any missing Traversable instances.

If you defined instance `Bitraversable Foo` but omitted instance `Traversable (Foo a)`, you must now define the latter.

You can do this by adding `{-# LANGUAGE DeriveTraversable #-}` at the top of the file and `deriving Traversable` at the data type definition. You should ensure that the new law is satisfied.
Alternatively you can explicitly define `traverse`. The following definition relies on an existing Bitraversable instance and ensures that the new law is satisfied:
```
class Traversable (Foo a) where
  traverse = bitraverse pure
```

   Example changes:

   - [Add `Traversable (UnderScope n f e)` instance in blanks defined via DeriveTraversable](https://github.com/ejconlon/blanks/pull/1/files)

   - [Added Traversable instances in futhark defined via `bitraverse pure`](https://github.com/diku-dk/futhark/pull/1955/files)

   - [Added Traversable instances in hgeometry and hgeometry-combinatorial](https://github.com/noinia/hgeometry/pull/210/files)

# PR template

Here is a template, which you can use when raising PRs against affected libraries.

> Title: Add Foldable/Traversble instance for _
>
> CLC has approved the proposal to make Foldable and Traversable quantified superclasses of 
> Bifoldable and Bitraversable respectively.
> (https://github.com/haskell/core-libraries-committee/issues/93).
> `forall a. Foldable (p a)` is now a superclass of `Bifoldable p` and
> `forall a. Traversable (p a)` is now a superclass of `Bitraversable p`.
>
> This means that you can not longer write `Bifoldable` or `Bitraversable` instances without
> a corresponding `Foldable` or `Traversable` instance.
>
> The implementation of the proposal is slated for GHC 9.8, but one can
> already future-proof code to be compliant with this change in a
> backwards-compatible way. No CPP required.
>
> Migration guide and more info:
> https://github.com/haskell/core-libraries-committee/blob/main/guides/bifoldable-bitraversable-superclass.md
