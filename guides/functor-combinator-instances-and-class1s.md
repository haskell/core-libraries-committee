# What

CLC has approved the [proposal #10](https://github.com/haskell/core-libraries-committee/issues/10), to relax instances for Functor combinators and put superclasses on \<class>1 and \<class2> to make the change less-breaking.
The effected classes are:

- `Eq1`, `Eq2`
- `Ord1`, `Ord2`
- `Show1`, `Show2`
- `Read1`, `Read2`

Before, e.g.:
```haskell
instance (Eq1 f, Eq1 g, Eq a) => Eq (Compose f g a)

class Eq1 f

class Eq2 f
```

After, e.g.:
```haskell
instance Eq (f (g a)) => Eq (Compose f g a)

class (forall a. Eq a => Eq (f a)) => Eq1 f

class (forall a. Eq a => Eq1 (f a)) => Eq2 f
```

# When

At the very earliest, the change may appear in GHC 9.6 (~ Q1 2023).

# How

This is a breaking change:

1. People can no longer define e.g. `Eq1` without `Eq`, or `Eq2` without `Eq1`

2. People can no longer write complicated instances on various abstract machinery that doesn't abide by the new superclasses.

However, the breakage is of limited scope.
An impact analysis (please see [comments following here](https://github.com/haskell/core-libraries-committee/issues/10#issuecomment-1166669613))
showed that only a few of the packages that sucessfully pass dependency resolution on Stackage `nightly-2022-06-17` break with this change.
Patches have been submitted to each of them.

The migration policy for this change is backwards-compatible: you can migrate already and still retain compatibility with existing GHCs.
Because of this, CLC suggests applying patches at your earliest convenience.

1. Add any missing instances

2. Sadly there is no exact pattern for this, but here are two examples:

   - https://github.com/haskell-streaming/streaming/pull/113
   - https://github.com/RyanGlScott/text-show/pull/57

   As one can see, this comes from rather fancy stuff.

   `streaming` could be simpler, as the pre-existing comment points out, by just requiring a `Show1 f` constraint.
   The other instance that required such a constraint did not need any migration.

   `text-show` had `*1` instances that also converted between `String`- and `Text`-based showing.
   This violates the superclass.
   The instance must instead "pass through" `String`-based shows to `String`-based shows.

Note what is *not* a breaking change is the changing of the instances for `Compose` etc.
Thanks to the new superclasses, the old instance constraints imply the new instance constraints, so existing generic code using those types will continue to work.
`Compose` and friends are much more common than the `*1` and `*2` classes, so this yields far less breakage overall.

# PR template

Here is a template for (1), which you can use when raising PRs against affected libraries.

> Title: Add instance _
>
> CLC has approved the proposal to add superclasses to `*1` and `*2`
> classes
> (https://github.com/haskell/core-libraries-committee/issues/10#). For
> example, `forall a. Eq a => Eq (f a)` is now a superclass of `Eq1 f`.
>
> This means that you can not longer write a `*1` instance without its
> regular class equivalent, and likewise a `*2` instances without its `*1`
> equivalent.
>
> The implementation of the proposal is slated for GHC 9.6, but one can
> already future-proof code to be compliant with this change in a
> backwards-compatible way. No CPP required.
>
> Migration guide and more info:
> https://github.com/haskell/core-libraries-committee/blob/main/guides/functor-combinator-instances-and-class1s.md
