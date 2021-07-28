package sbt.internal

import sbt.internal.util.appmacro.*
import verify.*
import ContextUtilTestMacro.*

object ContextUtilTest extends BasicTestSuite:
  test("extractTypeCon") {
    assert(
      extractTypeCon(true)
        == "TypeRef(ThisType(TypeRef(NoPrefix,module class immutable)),class List)"
    )
  }

  test("extractSingleton") {
    assert(extractSingleton(false) == "Ident(ListMonadInstance)")
  }

  test("makeLambda") {
    assert(makeLambda(())(false) == "false")
  }
end ContextUtilTest
