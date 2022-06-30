package ru.tinkoff.oolong

case class C(a: Int)
case class Test(a: Int, b: Long, c: C)

object Test extends App {
  inline given QueryMeta[C] = Meta.meta[C](_.a -> "aaaaaa")
  given QueryMeta[Test] = Meta.meta[Test](_.a -> "aaa", _.b -> "bbb", _.c -> "ccc")

  Meta.meta[Test](_.a -> "aaa", _.b -> "bbb", _.c -> "ccc").fun.keySet.head

}
