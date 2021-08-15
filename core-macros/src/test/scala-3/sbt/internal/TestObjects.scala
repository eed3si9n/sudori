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
    def mapN[K[L[a]]: AList, A2](in: K[List], f: K[Id] => A2): List[A2] =
      summon[AList[K]].apply(in, f)
end TestObjects
