package sbt.internal

import sbt.internal.util.appmacro.*
import verify.*
import ContextUtilTestMacro.*

object ContextUtilTest extends BasicTestSuite:
  test("extractTypeCon") {
    assert(
      someMacro(
        true
      ) == "AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class immutable)),class List),List(TypeParamRef(x)))"
    )
  }
end ContextUtilTest
