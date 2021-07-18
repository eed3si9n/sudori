package sbt.internal

import sbt.internal.util.appmacro.*

import verify.*
import ConvertTestMacro._

object ConvertTest extends BasicTestSuite:
  test("convert") {
    assert(someMacro(ConvertTest.wrapInit(1).toString == "Some(2)"))
  }

  def wrapInitTask[A](a: A): Int = 2
  def wrapInit[A](a: A): Int = 2
end ConvertTest
