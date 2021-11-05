# Core Libraries Committee

## Structure

The committee consists of 6 members:

* chessai @chessai (term ending January 2023)
* Cale Gibbard @cgibbard (term ending January 2023)
* Emily Pillmore @emilypi (term ending January 2023)
* George Wilson @gwils (term ending January 2023)
* Melanie Brown @cigsender (term ending January 2025)
* Andrew Lelechenko @Bodigrim (chair, term ending January 2025)

The committee makes decisions by simple majority voting.

## Contacts

Members of the public are suggested to contact CLC primarily via this issue tracker.
As a secondary medium with more limited visibility one can use a mail list
core-libraries-committee at haskell dot org
([public archives](https://groups.google.com/g/haskell-core-libraries)).

## `base` package

The primary responsibility of CLC is maintenance of `base` package (owned by GHC Team).
See more on the topic in `PROPOSALS.md`.

## Core Libraries

As a collective entity CLC owns, but does not maintain so-called Core Libraries:

* `array`,
* `bytestring`,
* `deepseq`,
* `directory`,
* `filepath`,
* `mtl`,
* `primitive`,
* `process`,
* `random`,
* `stm`,
* `template-haskell`,
* `text`,
* `unix`,
* `vector`,
* `Win32`.

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

### Suggesting changes to Core libraries

Because the CLC owns these libraries but does not maintain them, if you have an improvement
to suggest for a core library (including one that affects several), please start by contacting
the maintainer(s) and/or main source repositories, as listed on Hackage. If the maintainer
is unsure of whether to proceed, they can come to the CLC for further guidance. On the other
hand, if you struggle to reach a responsive maintainer, come to the CLC directly.
