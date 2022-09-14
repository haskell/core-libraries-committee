# Impact assessments

To provide a clearer picture of the downstream effects of a proposal, the CLC requires that proposers draft an impact assessment of their changes before a final decision is reached. An impact assessment MUST be completed if the proposal contains [breaking changes](https://pvp.haskell.org/), and is RECOMMENDED even if the changes are simply additive, especially in cases of anticipated name clashing or when re-exporting from the Prelude.

## The process

- Fork and build [a newer version of GHC](https://gitlab.haskell.org/ghc/ghc#building-installing) (say, the latest official release) that includes the changes made in your proposal. You should also make a merge request with your changes and add it to your proposal's issue.
- Try to build [clc-stackage](https://github.com/Bodigrim/clc-stackage) using your custom souped-up GHC. If anything breaks, point it out in your update, and notify the packages' maintainers (or raise a PR if you're able!). At least, point the maintainers to your proposal and describe the scope of the breakage.
- While not strictly the onus of the proposer, we encourage you to submit compatibility patches to broken libraries, to minimize library maintainers' time spent on "`base` churn" ;)
- Once everything that breaks receives a patch, let us know in a comment where they've been submitted. Then, the committee will be able to vote!
