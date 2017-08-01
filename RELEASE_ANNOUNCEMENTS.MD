We are glad to announce that cats 1.0.0-MF is released. 

`MF` stands for milestone final,
this will be the last release before cats 1.0.0-RC1 which will be followed by cats 1.0 shortly. 

The main purpose/focus of this release is to offer a relatively stable API to work with prior to the official 1.0.
It can be deemed as a proposal for the final cats 1.0 API. Please help test it and report any improvements/fixes
needed. Post cats 1.0, we will keep API stable and maintain strong binary compatibility. 

1.0.0-MF has over 120 merged pull requests coming from 44 contributors, 21 of them for the first time.

Highlights of the new features include but not limit to:

* [#1117](https://github.com/typelevel/cats/pull/1117): Stack safe `foldLeftM` without `Free`, by @TomasMikula 
* [#1598](https://github.com/typelevel/cats/pull/1598): A `ReaderWriterStateT` data type, by @iravid
* [#1526](https://github.com/typelevel/cats/pull/1526) and [#1596](https://github.com/typelevel/cats/pull/1596): `InjectK` for free programs. by @tpolecat and @andyscott
* [#1602](https://github.com/typelevel/cats/pull/1602): Stack-safe `Coyoneda`. by @edmundnoble
* [#1728](https://github.com/typelevel/cats/pull/1728): `As` class which represents subtyping relationships (`Liskov`). by @stew
* [#1178](https://github.com/typelevel/cats/pull/1178): `Is` constructor for Leibniz equality. by @tel
* [#1748](https://github.com/typelevel/cats/pull/1748): Stack-safe `FreeAppplicative`. by @edmundnoble
* [#1571](https://github.com/typelevel/cats/pull/1571): added `whileM`, `untilM`, `iterateWhile`, etc to `Monad`. by @tpolecat & @kailuowang

There are more breaking changes in this release - we want to include as many necessary API breaking changes as possible in this release 
to reach stability. 
Please follow the [migration guide][migration guilde] from 0.9.0. 

For the complete change list please go to our [release note][release note]. 

Although we made many improvements to the documentation in this release, it's still by and large a WIP.
The next release 1.0.0-RC1 will focus on API refinement and documentation. It will likely be the last 
release before cats 1.0. 

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
* @toplecat
* @tpolecat
* @wedens
* @xavier-fernandez
* @xuwei-k
* @yilinwei
* @zainab-ali 



[migration guilde]: 
[release note]:
