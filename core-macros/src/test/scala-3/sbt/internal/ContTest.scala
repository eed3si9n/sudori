package sbt.internal

import sbt.internal.util.appmacro.*
import verify.*
import ContTestMacro.*

object ContTest extends BasicTestSuite:
  test("pure") {
    assert(contMapNMacro[Int](12) == List(12))
  }

  test("getMap") {
    assert(contMapNMacro[Int](ContTest.wrapInit(List(1)) + 2).toString == "List(3)")
  }

  /*
  test("getMapN") {
    assert(
      contMapNMacro[Int](
        ContTest.wrapInit(List(1))
          + ContTest.wrapInit(List(2)) + 3
      ).toString == "List(3)"
    )
  }
   */

  // This compiles away
  def wrapInit[A](a: List[A]): A = ???
end ContTest
