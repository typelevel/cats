The cats maintainer team is proud to announce the **cats 1.0.0-MF** release. 

**MF** stands for *milestone final*,
this will be the last release before cats 1.0.0-RC1 which will be followed by cats 1.0 shortly. 

The main purpose/focus of this release is to offer a relatively stable API to work with prior to the official 1.0.
It can be deemed as a proposal for the final cats 1.0 API. Please help test it and report any improvements/fixes
needed either in the [cats-dev gitter channel](https://gitter.im/typelevel/cats-dev) or as [github issues](https://github.com/typelevel/cats/issues/new). 
Post cats 1.0, we will keep API stable and maintain strong binary compatibility. 

Highlights of the major new features include but not limit to:

* [#1117](https://github.com/typelevel/cats/pull/1117): Stack safe `foldLeftM` without `Free`, by @TomasMikula 
* [#1598](https://github.com/typelevel/cats/pull/1598): A `ReaderWriterStateT` data type, by @iravid
* [#1526](https://github.com/typelevel/cats/pull/1526) and [#1596](https://github.com/typelevel/cats/pull/1596): `InjectK` for free programs, by @tpolecat and @andyscott
* [#1602](https://github.com/typelevel/cats/pull/1602): Stack-safe `Coyoneda`, by @edmundnoble
* [#1728](https://github.com/typelevel/cats/pull/1728): `As` class which represents subtyping relationships (`Liskov`), by @stew
* [#1178](https://github.com/typelevel/cats/pull/1178): `Is` constructor for Leibniz equality, by @tel
* [#1748](https://github.com/typelevel/cats/pull/1748): Stack-safe `FreeAppplicative`, by @edmundnoble

Overall 1.0.0-MF has over 120 merged pull requests of API additions, bug fixes, documentation and misc
improvements from 44 contributors. For the complete change list please go to the [release notes][release notes]. 

### Migration
There are more breaking changes in this release - we want to include as many necessary breaking changes as possible in this release 
to reach stability. Please follow the [migration guide][migration guide] from 0.9.0 in the  [release notes][release notes]. 



### What's next

Although we made many improvements to the documentation in this release, it's still by and large a WIP.
The next release 1.0.0-RC1 will focus documentation and API refinement based on community feedback. 
RC1 is scheduled to release in September. Unless the amount of bug fixes warrants a RC2, it's likely that
we'll release cats 1.0.0 within a couple weeks after RC1. 


### Credits
Last but not least, many thanks to the contributors that make this release possible:

* @alexandru
* @andyscott
* @BenFradet
* @Blaisorblade
* @cb372
* @ceedubs
* @cranst0n
* @DavidGregory084
* @denisftw
* @DieBauer
* @diesalbla
* @djspiewak
* @durban
* @edmundnoble
* @iravid
* @jtjeferreira
* @julien-truffaut
* @jyane
* @kailuowang
* @larsrh
* @Leammas
* @leandrob13
* @LukaJCB
* @markus1189
* @milessabin
* @n4to4
* @oskoi
* @peterneyens
* @PeterPerhac
* @raulraja
* @RawToast
* @sellout
* @stew
* @sullivan-
* @SystemFw
* @takayuky
* @tel
* @TomasMikula
* @tpolecat
* @wedens
* @xavier-fernandez
* @xuwei-k
* @yilinwei
* @zainab-ali 



[migration guide]: 
[release notes]:
