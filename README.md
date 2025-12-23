# Core Libraries Committee

## Quick Links

* [Proposals under active consideration](https://github.com/haskell/core-libraries-committee/issues) are open issues.
* [Accepted proposals](https://github.com/haskell/core-libraries-committee/issues?q=is%3Aissue+is%3Aclosed+label%3Aapproved) are closed issues with the `approved` label.

## Our mission in brief

* Provide a smooth, but thorough process through which the community can participate in API changes to [base](#base-package) (the de-facto standard library for Haskell)
* Maintain a high standard for [respectful and productive discussions](#etiquette)
* Work closely with GHC developers to get base changes released quickly
* On request, assist the community with [Core Libraries](#core-libraries) by ensuring they have active maintainers
* On request, assist maintainers of said packages with counsel
* Co-maintain the [Haskell Package Versioning Policy](#pvp)

## Structure

The committee consists of 9 members:

* Julian Ospald @hasufell (**chair**, term ending February 2026)
* Matt Parsons @parsonsmatt (term ending February 2026)
* Josh Miller @velveteer (term ending September 2026)
* Teo Camarasu @TeofilC (**co-chair**, term ending February 2028)
* Daniel Casanueva @Daniel-Diaz (term ending February 2028)
* Philip Hazelden @ChickenProp (term ending February 2028)
* Jaro Reinders @noughtmare (term ending February 2028)
* Marc Scholten @mpscholten (term ending February 2028)
* Jeff Young @doyougnu (term ending February 2028)

The committee makes decisions by simple majority voting. CLC members are also allowed to submit their own [proposals][proposals] and vote on them.

## Contacts

Members of the public are suggested to contact CLC primarily via [this issue tracker](https://github.com/haskell/core-libraries-committee/issues).
As a secondary medium with more limited visibility one can use a mail list
core-libraries-committee at haskell dot org
([public archives](https://groups.google.com/g/haskell-core-libraries)).

## `base` package

The primary responsibility of CLC is to manage API changes of `base` package. The ownership of `base` belongs to GHC developers, and they can maintain it freely without CLC involvement as long as changes are invisible to clients. Changes which affect performance or laziness and similar are deemed visible. Documentation changes normally fall under GHC developers purview, except significant ones (e. g., adding or changing type class laws).
Proposals to change the API of the `base` package are managed by the process, described in [`PROPOSALS.md`][proposals].

[proposals]: https://github.com/haskell/core-libraries-committee/blob/main/PROPOSALS.md

## PVP

CLC (jointly with Hackage Admins) maintains [Haskell Package Versioning Policy](https://pvp.haskell.org/). Minor changes (fixing typos, clarifications, recommended practices) can be approved by CLC vote, while bigger, potentially breaking changes require agreement from Hackage Admins as well. Proposals should be raised at [PVP issue tracker](https://github.com/haskell/pvp/issues).

## Core Libraries

As a collective entity CLC owns, but does not maintain, so-called Core Libraries, which are libraries
* that are depended on by many other packages, and hence play a particularly important role in the Haskell ecosystem; and
* whose maintainers have agreed (or requested) support from the CLC in ensuring the long-term health of the library.

The overall goals are
* to reduce pressure on, and add support for, maintainers whose libraries have grown very popular; and
* to provide the community with a way to deal with the single-point-of-failure problem when a maintainer of a widely-used library becomes unavailable (perhaps for good reasons).

Current list of Core Libraries:

* `array`,
* `binary`,
* `bytestring`,
* `Cabal`,
* `Cabal-syntax`,
* `cabal-install`,
* `cabal-install-solver`,
* `deepseq`,
* `directory`,
* `entropy`,
* `file-io`
* `filepath`,
* `haskeline`,
* `hpc`,
* `hpc-bin`,
* `mtl`,
* `os-string`,
* `primitive`,
* `process`,
* `random`,
* `stm`,
* `template-haskell`,
* `terminfo`,
* `text`,
* `transformers`,
* `unix`,
* `vector`,
* `vector-binary-instances`,
* `Win32`,
* `xhtml`.

Maintainers of Core Libraries may at their own accord seek CLC approval for
controversial changes, but are not required to do so. CLC does not interfere
with daily development of Core Libraries as long as appointed maintainers
keep them in an appropriate shape and support healthy communication with
contributors. If CLC finds a Core Library in a neglected state, it can
source and install new maintainers or resolve situation by other means.

Libraries that wish CLC to help them with sourcing maintainers
may apply for adoption by CLC.

Core Libraries are different from GHC boot libraries: some boot libraries are not Core
(and as such are maintained either solely by GHC Team, or by third parties), and some
Core Libraries are not required to boot GHC.

Questions related to management of Core Libraries are usually discussed in respective issue trackers. Please bring them to CLC attention by CC'ing CLC members or pinging by email to core-libraries-committee at haskell dot org.

### Suggesting changes to Core libraries

Because the CLC owns these libraries but does not maintain them, if you have an improvement
to suggest for a core library (including one that affects several), please start by contacting
the maintainer(s) and/or main source repositories, as listed on Hackage. If the maintainer
is unsure of whether to proceed, they can come to the CLC for further guidance. On the other
hand, if you struggle to reach a responsive maintainer, come to the CLC directly.

### Elections

Vacant or expired CLC member seats are filled via elections. Anyone
can nominate themselves and outgoing CLC members can re-nominate themselves.

Usually a nomination process takes place during January, followed by a vote in early February.
If a position becomes vacant in the middle of the year, it can be filled immediately
or left vacant until a new cycle. The danger is that if you have an election to fill
a single slot in October followed by a regular election to fill, say, three slots in January,
the former will be overbooked and the latter is likely to offer a very limited choice.

Elections kick off with a chair posting a call for nominations on various community platforms.
The chair creates a separate private mailbox to gather nominations
(do not use public mail lists for this purpose)
and shares received messages with the rest of the committee members,
again using private emails.

Depending on the number of slots to fill and nominations received, the chair
recommends a voting procedure. Unless there is a reason to diverge,
[Condorcet procedure](https://en.wikipedia.org/wiki/Condorcet_method)
with [Schulze method](https://en.wikipedia.org/wiki/Schulze_method)
is used.

After elections the following groups should be updated and kept in sync:

* [GitHub team](https://github.com/orgs/haskell/teams/core-libraries-committee)
* [GitLab team](https://gitlab.haskell.org/groups/core-libraries/-/group_members)
* [Maillist members](https://groups.google.com/g/haskell-core-libraries/members)

## Etiquette

Everyone is welcome to participate in discussions and proposals. We expect all participants
to be respectful and on-topic.

Proposers should also be aware that alternative solutions and designs are generally on-topic,
as they may affect the voting. While it's up to the proposer on how much they engage in
alternative design discussions, we consider these vital to the proposal process.

## Our values

These are the qualities and principles that we do our best to informally commit to when making decisions
and handling contributions.

### Good engineering

Since `base` is the standard library of Haskell, we believe that proposed changes need extra
scrutiny and careful consideration. Our main objective is to assess whether a design is technically
sound and the trade-offs are something we can commit to.

This sometimes requires lengthy discussions, experiments, and additional proof of correctness.

### Open collaboration

Anyone in the community is welcome to participate in the proposal process, regardless of their background
or opinions, as long as they follow [proper etiquette](#etiquette).

We believe that in order to maintain such a central piece of software it is paramount to be as welcoming as possible
to collaboration and new ideas. We seek to assess these ideas only on their merits, no matter where they come from,
and to provide a smooth, consistent, and predictable contribution experience.

### Truth seeking

During difficult discussions, we commit to both understanding each participant's perspective as well as establishing irrefutable facts.

While each individual's priorities may vary, our evaluation should always be based on technical merit and not on
social credit or personal bias.

Any participant, no matter their status in the Haskell community, should be able to call out incorrect assumptions
and faulty reasoning. We do not shame people for being wrong, but we expect that we can progress to finding
good answers as a collective.

