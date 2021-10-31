package sbt.internal

import sbt.internal.util.appmacro.*
import sbt.internal.util.AList
import sbt.internal.util.Classes.Applicative
import sbt.internal.util.Types.Id

import scala.concurrent.Future

object TestObjects:
  import concurrent.ExecutionContext.Implicits.*

  object ListMonadInstance extends MonadInstance:
    type F[x] = List[x]
    def pure[A](in: () => A): List[A] = List(in())
    def flatten[A](in: List[List[A]]): List[A] = in.flatten
    def map[A1, A2](in: List[A1], f: A1 => A2): List[A2] = in.map(a1 => { f(a1) })
    def mapN[Tup <: Tuple, A2](in: Tuple.Map[Tup, List], f: Tup => A2): F[A2] =
      TupleUtil.tuple.mapN[List, A2, Tup](in, f)

  given futureApplicative: Applicative[Future] with
    def pure[A](a: => A): Future[A] = Future { a }
    def map[A1, A2](f: A1 => A2, value: Future[A1]): Future[A2] =
      value.map(f)
    def apply[A1, A2](f: Future[A1 => A2], value: Future[A1]): Future[A2] =
      value.zipWith(f) { case (a, f) =>
        f(a)
      }
end TestObjects
