package sbt.internal

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.*

import sbt.internal.util.appmacro.*
import verify.*
import TestObjects.futureApplicative
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object TupleUtilTest extends BasicTestSuite:

  val t1 = ((Option(1), Option("foo")))
  def t2 = (
    Future {
      println("started 1")
      Thread.sleep(100)
      1
    },
    Future {
      println("started 2")
      Thread.sleep(100)
      "foo"
    },
  )

  val tupleUtil = TupleUtil.tuple

  test("transform") {
    val f = [A] => (x: Option[A]) => x.toList
    assert(
      tupleUtil.transform[Option, List, (Int, String)](t1, f) == (List(1), List("foo"))
    )
  }

  test("traverse") {
    val f = [A] =>
      (x: Option[A]) =>
        x match
          case Some(x: Int) => List(x + 1).asInstanceOf[List[A]]
          case _            => List(x.get)
    val actual = tupleUtil.traverse[Option, List, (Int, String)](t1, f)
    assert(actual == List((2, "foo")))
  }

  test("mapN") {
    val tuple = t2
    val f = (arg: (Int, String)) => arg._1.toString + "|" + arg._2
    val actual = tupleUtil.mapN[Future, String, (Int, String)](tuple, f)
    val result = Await.result(actual, Duration.Inf)
    assert(
      result.toString == "1|foo"
    )
  }

  test("traverseX") {
    val f = [A] => (x: Option[A]) => List(List(x.get))
    val actual = tupleUtil.traverseX[Option, List, List, (Int, String)](t1, f)
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
end TupleUtilTest
