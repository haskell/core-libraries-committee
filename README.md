# Core Libraries Committee

## Quick Links

* [Proposals under active consideration](https://github.com/haskell/core-libraries-committee/issues) are open issues.
* [Accepted proposals](https://github.com/haskell/core-libraries-committee/issues?q=is%3Aissue+is%3Aclosed+label%3Aapproved) are closed issues with the `approved` label.

## Structure

The committee consists of 7 members:

* Melanie Phoenix @mixphix (term ending January 2025)
* Andrew Lelechenko @Bodigrim (chair, term ending January 2025)
* Tom Ellis @tomjaguarpaw (term ending February 2025)
* Moritz Angermann @angerman (term ending February 2026)
* Julian Ospald @hasufell (term ending February 2026)
* Matt Parsons @parsonsmatt (term ending February 2026)
* Josh Miller @velveteer (term ending September 2026)

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
* `bytestring`,
* `deepseq`,
* `directory`,
* `entropy`,
* `file-io`
* `filepath`,
* `mtl`,
* `os-string`,
* `primitive`,
* `process`,
* `random`,
* `stm`,
* `template-haskell`,
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
