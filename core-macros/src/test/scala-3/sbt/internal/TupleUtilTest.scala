package sbt.internal

import sbt.internal.util.appmacro.*
import verify.*
import scala.concurrent.Future

object TupleUtilTest extends BasicTestSuite:

  val t1 = ((Option(1), Option("foo")))
  val tupleUtil = TupleUtil.tuple

  test("transform") {
    val f = [A] => (x: Option[A]) => x.toList
    assert(
      tupleUtil.transform[Option, List, (Int, String)](t1, f) == (List(1), List("foo"))
    )
  }

  test("traverse") {
    val f = [A] => (x: Option[A]) => List(List(x.get))
    val actual = tupleUtil.traverse[Option, List, List, (Int, String)](t1, f)
    Predef.assert(
      actual.toString == "List((List(1),List(foo)))",
      s"""
      actual: $actual
      """
    )
  }

  test("foldr") {
    val f = [A] => (x: Option[A], acc: String) => x.toString + acc
    assert(
      tupleUtil.foldr[Option, String, (Int, String)](t1, f, "") == "Some(1)Some(foo)"
    )
  }

  test("toList") {
    val actual = tupleUtil.toList[Option, (Int, String)](t1)
    assert(
      actual == List(Some(1), Some("foo"))
    )
  }

  test("ap") {
    val f = (arg: (Int, String)) => arg._1.toString + "|" + arg._2
    val actual = tupleUtil.ap[Option, String, (Int, String)](t1, f)
    assert(
      actual == Some("1|foo")
    )
  }
end TupleUtilTest
