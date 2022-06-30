package ru.tinkoff.oolong

case class C(a: Int)
case class Test(a: Int, b: Long, c: C)

@main def run = println(Meta.meta[Test](_.a -> "aaa", _.b -> "bbb", _.c -> "ccc"))
