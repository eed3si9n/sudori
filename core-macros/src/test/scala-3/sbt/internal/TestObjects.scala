package sbt.internal

import sbt.internal.util.appmacro.*

object TestObjects:
  object ListMonadInstance extends MonadInstance:
    type F[x] = List[x]
    def pure[A](in: () => A): List[A] = List(in())
    def flatten[A](in: List[List[A]]): List[A] = in.flatten
end TestObjects
