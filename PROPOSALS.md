# Proposals

Want to make a change to `base`? Great! We'd love to hear your suggestion.
Just follow the steps outlined here so that your idea doesn't lose traction!

TL;DR Quick overview of the proposal lifecycle:

1. [Create an issue][open-issue] with your idea.
2. (When requested) Implement the proposal.
3. (If requested) Make [impact assessment](#impact-assessments) (it's mandatory for breaking changes).
4. (If requested) Let us know in a comment about patches to impacted libraries.
5. Wait for the vote.
6. (If accepted) Write a migration guide (for proposals with breaking changes).

## The "what"

If you have an idea for a _language extension_ or _compiler warning_,
you should follow the steps for the **GHC proposal process** instead of this one.
Those are compiler-specific features that are outside the focus of the CLC.

If you've got insight into how to improve performance, behaviour,
or structure of `base`, awesome! This is the stuff we'd love to hear about.
You should follow the steps below, keeping in mind that the bigger
the scope of your proposal, the more detailed it should be.

## The "how"

Different ideas have different scales and scopes.
Not every proposal will fit into the same rigid format,
and it's discouraging to be told that you have to write a formal document
when all you want to do is make an optimization to a function or two.
However, a tremendous idea will only be implemented if you can convince others it's good, too.
Here's what you can do to help make that happen:

- [Open an issue][open-issue]
    at the Core Libraries Committee GitHub page.

    - This will be the centre of discussion for your proposal.
        Related discussions, including those with CLC members, will happen here;
        merge requests to `base` and other related actions
        will reference this issue as they are taken.
    - The title should clearly state the end goal of your proposal. For example:

        - "Replace all occurrences of `return` with `pure`"
        - "Factor out helper function from `functionName`"
        - "Improve asymptotic complexity of `sort`"

    - Write a paragraph or so (proportional to the scope) about what your proposal
        aims to achieve and why you want it implemented. Sell it to us!
        Here's some prompts to help you get started:

        - What's wrong with how it is now?
        - Did you consider workarounds? What did/didn't work?
        - What would you be able to do if it was merged today?

    - If you've got some draft changes ready, you can include some excerpts here,
        before setting up a pull request.

    - If your proposal includes breaking changes, you **must** include an [impact assessment](#impact-assessments). A [migration guide](https://github.com/haskell/core-libraries-committee/tree/main/guides) may also be requested if the breakage affects many, or critical, libraries.

The issue creator is responsible for seeing their proposal through.
The Core Libraries Committee's focus is around management,
not implementation: you should do the work for your own proposals
(or others' if you like them!) to show us that you think it's worth doing.

To be more precise, as the proposal author, you will be expected to do the following
tasks, or find volunteers who'll do this for you. You may post a brief proposal and
request feedback on feasibility before investing time and effort in the full process.

1. Refine your proposal.
2. Make impact assessment.
3. Write a migration guide.
4. Do some work on writing migration patches. Patches can be provided in several forms:
    * raise draft PRs to affected packages, but communicate clearly that
      the proposal is not accepted yet,
    * create fork repositories with patched packages
      and a `cabal.project` template using them,
    * or just share patch files, in the same format as
      [`head.hackage`](https://gitlab.haskell.org/ghc/head.hackage) does.
5. Implement the proposal.
    * Register at https://gitlab.haskell.org/ghc/ghc.
    * If your account is not approved soon (this is a manual process to combat spam),
      ask for approval in your CLC discussion thread, someone will take care of it.
    * Fork GHC repository and implement your proposal in a non-`master` branch.
      (It's probably best if you create a new branch rather than working in `master`, see below.)
    * Follow [GHC's "Contributing a Patch" guidelines](https://gitlab.haskell.org/ghc/ghc/-/wikis/Contributing-a-Patch)
      when creating an MR (e.g. [appropriate commit messages](https://gitlab.haskell.org/ghc/ghc/-/wikis/Contributing-a-Patch#22-commit-messages)).
    * Make sure to include a link to the CLC discussion into the commit message.
    * Make sure to update `libraries/base/changelog.md`. (You might like to insert your new entry
      at an arbitrary position instead of the last one to minimise merge conflicts.)
    * Raise a merge request (MR). Ensure your merge request is not from a branch
      called `master` because that will cause issues for Marge Bot. (A GitLab merge
      request works similarly to what GitHub calls a "pull request" (PR).)
    * Do not mark your MR as "draft", you actually want people to pay attention and
      review as early as possible. "Draft" is often understood by potential reviewers
      as a "private experiment".
    * Writing tests is always a good idea and especially crucial for
      semantic changes, if feasible.
    * If you happen to have rights to assign labels, label the MR as "core libraries"
      and "needs CLC feedback".
    * If your proposal modifies types of existing entities or adds new entities, you
      likely need to adjust baselines in `testsuite/tests/interface-stability`. Usually
      you can do it by running `hadrian/build -j test --only=base-exports --test-accept`
      and copying changes to adjacent files.

## The "when"

If you've got a pet issue that's been sleeping in the depths of a mailing list
for a while, and you think everyone's forgotten about it, now's your chance!
Double-check the issues page in case someone else wants the same thing.
If they do, upvote that one instead of making your own! Otherwise, it's your time to shine;
write up a proposal and we'll be sure to give it due process.

But alas, I hear you sigh, what if it gets abandoned _again_?
Well, here is where issue trackers outshine mailing lists:
keeping track of long-running projects is the entire point!
Typically the CLC will wait to start its internal voting process
until public discussion about the proposal settles, but if
the issue creator or main contributors find that too much bikeshedding
is happening and wants to reach a verdict, they can choose to
trigger the vote manually ("hey CLC, I'd like you to vote on this now :)").
Once the voting process has begun or there have been no new comments
on an issue for 7 days, the CLC will have 14 days to reach a conclusion,
or reach out for further consideration if we can't get to one as it stands.

## The "who"

We aspire to respond to comments, pull requests, and direct emails
no more than 14 days after their arrival. We'd like to make sure that
the Haskell community feels they can hold us accountable for this.

To this end, if you believe that we are taking too long to respond,
that the CLC is not doing its due diligence in monitoring active issues,
or have other concerns, we think it's fair that the [GHC Development team](https://gitlab.haskell.org/ghc/ghc/-/wikis/team-ghc),
the owners of the `base` library, should have the power to withdraw custodianship
from the committee.

## Impact assessments

To provide a clearer picture of the downstream effects of a proposal, the CLC requires that proposers draft an impact assessment of their changes before a final decision is reached. An impact assessment MUST be completed if the proposal contains [breaking changes](https://pvp.haskell.org/), and is RECOMMENDED even if the changes are simply additive, especially in cases of anticipated name clashing or when re-exporting from the Prelude.

- Fork and build [a newer version of GHC](https://gitlab.haskell.org/ghc/ghc#building-installing) (say, the latest official release) that includes the changes made in your proposal. You should also make a merge request with your changes and add it to your proposal's issue.
- Try to build [`clc-stackage`](https://github.com/Bodigrim/clc-stackage) using your custom souped-up GHC. Point out important breakages in your update.
- Once everything that breaks receives a patch, let us know in a comment where they've been submitted. Then, the committee will be able to vote!
- If your proposal is approved, we encourage you to raise pull requests on the affected packages. Making it easy for maintainers to adapt to new changes increases the effectiveness of the proposal process :)

For more details, visit the [`clc-stackage` how-to](https://github.com/Bodigrim/clc-stackage#how-to).

## Proposal lifecycle

Your proposal may move through different stages which is indicated by the corresponding label:

* ![awaits-proposal][label:awaits-proposal]
  Your proposal should be written with more specifics to produce an actionable item.
* ![awaits-ghc-proposal][label:awaits-ghc-proposal]
  GHC should be changed before further movement on your proposal.
* ![awaits-impact-assessment][label:awaits-impact-assessment]
  You need to perform impact assessment so CLC can better understand the
  breakage impact.
* ![awaits-MR][label:awaits-MR]
  You need to implement the proposal in `base` because it's hard to vote before
  seeing the actual implementation.
* ![approved][label:approved]
  Congratulations! Your proposal is approved. Now you need to implement it (if
  you haven't done it yet) and help others migrate their code in case of
  breaking changes.
* ![declined][label:declined]
  Your proposal has been declined. At this point, you don't need to do anything
  with it.
* ![out-of-scope][label:out-of-scope]
  Your proposal is not a subject of a CLC vote.
* ![withdrawn][label:withdrawn]
  You decided to not pursue the proposal explicitly.
* ![abandoned][label:abandoned]
  The proposal author is MIA and nobody has taken the proposal over.

<!-- Helpful links -->
[open-issue]: https://github.com/haskell/core-libraries-committee/issues/new

<!-- Labels -->
[label:awaits-proposal]: https://img.shields.io/github/labels/haskell/core-libraries-committee/awaits-proposal
[label:awaits-ghc-proposal]: https://img.shields.io/github/labels/haskell/core-libraries-committee/awaits-ghc-proposal
[label:awaits-impact-assessment]: https://img.shields.io/github/labels/haskell/core-libraries-committee/awaits-impact-assessment
[label:awaits-MR]: https://img.shields.io/github/labels/haskell/core-libraries-committee/awaits-MR
[label:approved]: https://img.shields.io/github/labels/haskell/core-libraries-committee/approved
[label:declined]: https://img.shields.io/github/labels/haskell/core-libraries-committee/declined
[label:out-of-scope]: https://img.shields.io/github/labels/haskell/core-libraries-committee/out-of-scope
[label:withdrawn]: https://img.shields.io/github/labels/haskell/core-libraries-committee/withdrawn
[label:abandoned]: https://img.shields.io/github/labels/haskell/core-libraries-committee/abandoned
