# Deprecation process

## Introduction

Sometimes it is necessary to remove definitions from `base`, for example because
they expose GHC-internal implementation details that belong in `ghc-internal` or
`ghc-experimental` (see [GHC's base libraries: Combining stability with
innovation](https://github.com/haskellfoundation/tech-proposals/blob/main/proposals/accepted/051-ghc-base-libraries.rst)
for background on these packages and the broader context).

In order to save time and effort for both the CLC and GHC developers, this
document describes a streamlined standard workflow for deprecation and removal
of definitions. (This document refers to "definitions" for brevity, but is
intended to apply to whole modules and re-exports of existing definitions as
well.)  This functions as an alternative to the [main proposals
process](./PROPOSALS.md), intended for cases where the definitions being removed
are manifestly GHC-internal, have a clearly preferable replacement or are
otherwise unlikely to be controversial. The normal process should be used
instead for changes that require additional justification (e.g. deprecating a
widely-used function in a module outside the `GHC.*` namespace).

Proposals may be batched, e.g. by:
 - domain (e.g. all of `GHC.Stack.*`);
 - Hackage users (few Hackage users indicates it's easier to vote yes);
 - deprecation period;
 - how "internal" they are perceived.

If a batch proposal fails, it's fine to split it into multiple proposals. In
general, one proposal per module would seem too fine a granularity.

## Process details

The key idea of this process is to separate the decision-making stage (for which
CLC oversight is required) from the implementation stage (which happens over
time, without need for further input from the CLC).

### Decision stage

* The proposer [creates an issue](https://github.com/haskell/core-libraries-committee/issues/new)
  identifying the definitions to be deprecated and removed, referencing this
  policy.  The proposal should include an estimate of the number of Hackage
  packages that will be impacted by the change, and the duration of the proposed
  deprecation period.  However, an implementation MR or full impact assessment
  is not required at this stage.

* CLC members or other interested parties comment on the issue. In particular,
  they may raise concerns for specific functions, saying "we actually want those
  in `base`", and propose a new home for those functions (which module exactly).

* At least two weeks after the issue creation, the proposer updates the issue
  description with the final version of the proposal following the discussion,
  and asks the CLC to vote.

* The CLC votes on the proposal. If it is rejected, the process stops. If it is
  accepted, the GHC developers may proceed to the implementation section below.

### Implementation stage

All changes during the implementation stage are subject to the usual GHC MR
review process. The MRs should link to the CLC issue agreeing to the
deprecation. The CLC are welcome, but not obliged, to comment on implementation MRs.

* The implementer adds Haddock comments and `DEPRECATED` pragmas to the affected
  definitions. documenting that they are scheduled for removal. The Haddock
  comments should include:

   * the planned last GHC and/or `base` version that will allow to import this module; and

   * the migration strategy (different module, different package, different functions...),
     or if there is no direct migration possible, an explanation why.

* The deprecation period starts when a `base` release including deprecation
  comments is published.

* During the deprecation period, if it comes to light that there is a compelling
  reason to reverse the deprecation, e.g. due to feedback from users, this
  should be brought to the attention of the GHC developers via the GHC issue
  tracker. They may choose to extend the deprecation period or solicit CLC
  feedback.

* Deprecated definitions can be removed by the GHC developers in any major
  release of `base` published after the deprecation period has elapsed.  The
  `base` changelog should mention the definitions that have been removed, and
  the migration strategy.

## Deprecation periods

Each proposal will include a minimum deprecation period, typically based on the
following guidelines:

 * definitions that occur in fewer than 50 packages on Hackage need a deprecation
   period of at least 6 months / one major GHC release;

 * definitions that occur in 50-200 packages on Hackage need a deprecation period of at
   least 1 year / two major GHC releases;

 * definitions that occur in more than 200 packages on Hackage need a deprecation
   period of at least 2 years / four major GHC releases.

Longer deprecation periods may be appropriate if the removal involves
widely-used or very generic functions.
