package cats
package tests

import data.{OptionT,XorT,WriterT,StreamingT}

class MonadTransTests extends CatsSuite {

  test("monadTrans syntax on monad works"){

    val x: OptionT[List, Int] = List(1).liftM[OptionT]
    x.value should === (List(Option(1)))
  }

  test("we have monadTrans for XorT, OptionT, StreamingT, WriterT"){
    val a: WriterT[List, Int, Int] = List(1).liftM[({type λ[α[_], β] = WriterT[α, Int, β]})#λ]
    val b: StreamingT[List, Int]   = List(1).liftM[StreamingT]
    val c: OptionT[List, Int]      = List(1).liftM[OptionT]
    val d: XorT[List, Int, Int]    = List(1).liftM[({type λ[α[_], β] = XorT[α, Int, β]})#λ]
  }
}
