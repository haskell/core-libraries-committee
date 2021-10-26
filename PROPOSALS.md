# Proposals

Want to make a change to `base`? Great! We'd love to hear your suggestion. Just follow the steps outlined here so that your idea doesn't lose traction!

## The "what"

If you have an idea for a _language extension_ or _compiler warning_, you should follow the steps for the **GHC proposal process** instead of this one. Those are compiler-specific features that are outside the focus of the CLC.

If you've got insight into how to improve documentation, performance, behaviour, or structure of the _source code_ of `base`, awesome! This is the stuff we'd love to hear about. You should follow the steps below, keeping in mind that the bigger the scope of your proposal, the more detailed it should be.

## The "how"

Different ideas have different scales and scopes. Not every proposal will fit into the same rigid format, and it's discouraging to be told that you have to write a formal document when all you want to do is make an optimization to a function or two. However, a tremendous idea will only be implemented if you can convince others it's good, too. Here's what you can do to help make that happen:

1. [Open an issue](https://github.com/haskell/core-libraries-committee/issues/new) at the Core Libraries Committee GitHub page.

    - This will be the centre of discussion for your proposal. Related discussions, including those with CLC members, will happen here; further actions, such as PRs to the relevant codebases, will reference this issue as they are taken.
    - The title should clearly state the end goal of your proposal. For example:

        - "Replace all occurrences of `return` with `pure`"
        - "Factor out helper function from `functionName`"
        - "Improve asymptotic efficiency of `sort`"

    - Write a paragraph or so (proportional to the scope) about what your proposal aims to achieve and why you want it implemented. Sell it to us! Here's some prompts to help you get started:

        - What's wrong with how it is now?
        - Did you consider workarounds? What did/didn't work?
        - What would you be able to do if it was merged today?

    - If you've got some draft changes ready, you can include some excerpts here, before setting up a pull request.

2. Send an email to `core-libraries-committee@haskell.org` containing a link to the issue you created.

    - This is just to make darn sure we've seen the issue. Give us a gist and we'll get back to you on the issue page!

## The "when"

If you've got a pet issue that's been sleeping in the depths of a mailing list for a while, and you think everyone's forgotten about it, now's your chance! Double-check the issues page in case someone else wants the same thing. If they do, upvote that one instead of making your own! Otherwise, it's your time to shine; write up a proposal and we'll be sure to give it due process.

But alas, I hear you sigh, what if it gets abandoned _again_? Well, here is where issue trackers outshine mailing lists: keeping track of long-running projects is the entire point! In addition: once a proposal has been submitted, **it must be halted manually: lack of feedback from any CLC member _within 7 days_ should be interpreted as an all-clear to move forward**.

What does that mean for your project? If it's been **two weeks** since anyone raised concerns *that haven't been addressed*, you as the issue creator should take the initiative and create the pull request for the library you intend to change. If you don't have access (such as on GitLab), ping us individually in a reply to your issue and one of us will get back to you.

## The "who"

In the past the CLC has been criticized for its inefficiency and for having difficulties coordinating its members. We aspire to respond to comments, pull requests, and direct emails no more than 14 days after their arrival. We'd like to make sure that the Haskell community feels they can hold us accountable for this.

To this end, if you believe that we are taking too long to respond, that the CLC is not doing its due diligence in monitoring active issues, or have other concerns, we think it's fair that the [GHC Development team](link), the largest contributor to `base`, should have the power to reprimand the CLC when necessary.
