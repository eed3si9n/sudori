package sbt.internal

import sbt.internal.util.appmacro.*
import sbt.internal.util.AList
import sbt.internal.util.Types.Id

object TestObjects:
  object ListMonadInstance extends MonadInstance:
    type F[x] = List[x]
    def pure[A](in: () => A): List[A] = List(in())
    def flatten[A](in: List[List[A]]): List[A] = in.flatten
    def map[A1, A2](in: List[A1], f: A1 => A2): List[A2] = in.map(a1 => { f(a1) })
    def mapN[Tup <: Tuple, A2](in: Tuple.Map[Tup, List], f: Tup => A2): F[A2] =
      TupleUtil.tuple.ap[List, A2, Tup](in, f)
end TestObjects
