package sbt.internal

import sbt.internal.util.appmacro.*

object TestObjects:
  object ListMonadInstance extends MonadInstance:
    type F[x] = List[x]
    def pure[A](in: () => A): List[A] = List(in())
    def flatten[A](in: List[List[A]]): List[A] = in.flatten
    def map[A1, A2](in: F[A1], f: A1 => A2): F[A2] = in.map(a1 => { f(a1) })
end TestObjects
