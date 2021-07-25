package sbt.internal

import sbt.internal.util.appmacro.*
import verify.*
import ContTestMacro.*

object ContTest extends BasicTestSuite:
  test("pure") {
    assert(someMacro(12) == List(12))
  }
end ContTest
