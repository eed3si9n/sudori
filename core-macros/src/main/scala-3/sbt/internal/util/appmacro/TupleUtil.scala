package sbt.internal.util
package appmacro

import scala.Tuple.IsMappedBy
import Classes.Applicative
import Types._

/**
 * An abstraction over structured Tuple type constructor `X1[f[a]]`, previously known as AList.
 */
trait TupleUtil:
  import TupleUtil.idPoly

  def transform[F1[_], F2[_], Tup <: Tuple](
      value: Tuple.Map[Tup, F1],
      f: [a] => F1[a] => F2[a]
  ): Tuple.Map[Tup, F2]

  def traverse[F1[_], F2[_]: Applicative, Tup <: Tuple](
      value: Tuple.Map[Tup, F1],
      f: [a] => F1[a] => F2[a]
  ): F2[Tup]

  def mapN[F1[_]: Applicative, A, Tup <: Tuple](value: Tuple.Map[Tup, F1], f: Tup => A): F1[A] =
    summon[Applicative[F1]].map(f, traverse[F1, F1, Tup](value, idPoly[F1]))

  def traverseX[F1[_], F2[_]: Applicative, P[_], Tup <: Tuple](
      value: Tuple.Map[Tup, F1],
      f: [a] => F1[a] => F2[P[a]]
  ): F2[Tuple.Map[Tup, P]]

  def foldr[F1[_], A, Tup <: Tuple](
      value: Tuple.Map[Tup, F1],
      f: [a] => (F1[a], A) => A,
      init: A
  ): A

  def toList[F1[_], Tup <: Tuple](value: Tuple.Map[Tup, F1]): List[F1[Any]] =
    val f = [a] => (p1: F1[a], p2: List[F1[Any]]) => p1.asInstanceOf[F1[Any]] :: p2
    foldr[F1, List[F1[Any]], Tup](value, f, Nil)
end TupleUtil

object TupleUtil:
  type Tail[X <: Tuple] <: Tuple = X match
    case _ *: xs => xs

  def idPoly[F1[_]] = [a] => (p: F1[a]) => p

  def nil[Tup <: Tuple] = EmptyTuple.asInstanceOf[Tup]

  lazy val tuple: TupleUtil = new TupleUtil {
    override def transform[F1[_], F2[_], Tup <: Tuple](
        value: Tuple.Map[Tup, F1],
        f: [x] => F1[x] => F2[x]
    ): Tuple.Map[Tup, F2] =
      value match
        case _: Tuple.Map[EmptyTuple, F1] => nil[Tuple.Map[Tup, F2]]
        case (head: F1[x] @unchecked) *: tail =>
          (f(head) *: transform[F1, F2, Tail[Tup]](tail.asInstanceOf, f))
            .asInstanceOf[Tuple.Map[Tup, F2]]

    override def traverse[F1[_], F2[_]: Applicative, Tup <: Tuple](
        value: Tuple.Map[Tup, F1],
        f: [a] => F1[a] => F2[a]
    ): F2[Tup] =
      val F2 = summon[Applicative[F2]]
      value match
        case _: Tuple.Map[EmptyTuple, F1] => F2.pure(nil[Tup])
        case (head: F1[x] @unchecked) *: (tail: Tuple.Map[Tail[Tup], F1] @unchecked) =>
          val tt = traverse[F1, F2, Tail[Tup]](tail, f)
          val g = (t: Tail[Tup]) => (h: x) => (h *: t).asInstanceOf[Tup]
          F2.apply[x, Tup](F2.map(g, tt), f(head))

    override def traverseX[F1[_], F2[_]: Applicative, P[_], Tup <: Tuple](
        value: Tuple.Map[Tup, F1],
        f: [a] => F1[a] => F2[P[a]]
    ): F2[Tuple.Map[Tup, P]] =
      val F2 = summon[Applicative[F2]]
      value match
        case _: Tuple.Map[EmptyTuple, F1] => F2.pure(nil[Tuple.Map[Tup, P]])
        case (head: F1[x] @unchecked) *: (tail: Tuple.Map[Tail[Tup], F1] @unchecked) =>
          val tt = traverseX[F1, F2, P, Tail[Tup]](tail, f)
          val g = (t: Tuple.Map[Tail[Tup], P]) =>
            (h: P[x]) => (h *: t).asInstanceOf[Tuple.Map[Tup, P]]
          F2.apply[P[x], Tuple.Map[Tup, P]](F2.map(g, tt), f(head))

    override def foldr[F1[_], A, Tup <: Tuple](
        value: Tuple.Map[Tup, F1],
        f: [a] => (F1[a], A) => A,
        init: A
    ): A =
      value match
        case _: Tuple.Map[EmptyTuple, F1] => init
        case (head: F1[x] @unchecked) *: tail =>
          f(head, foldr[F1, A, Tail[Tup]](tail.asInstanceOf[Tuple.Map[Tail[Tup], F1]], f, init))
  }
end TupleUtil
