package ru.tinkoff.oolong.bson.derivation

import java.time.Instant
import java.time.Year

import ru.tinkoff.oolong.bson.BsonDecoder
import ru.tinkoff.oolong.bson.BsonEncoder
import ru.tinkoff.oolong.bson.annotation.BsonKey
import ru.tinkoff.oolong.bson.given

case class TestMeta(time: Instant, seq: Long, flag: Boolean) derives BsonEncoder, BsonDecoder

case class TestCheck(year: Year, comment: String) derives BsonEncoder, BsonDecoder

case class TestEntity(
    @BsonKey("_id") id: Int,
    name: String,
    meta: TestMeta,
    comment: Option[String],
    linkId: Option[Int],
    checks: Seq[TestCheck]
) derives BsonEncoder,
      BsonDecoder

case class TestContainer[T](value: Option[T]) derives BsonEncoder, BsonDecoder

case class TestEntityWithDefaults(
    @BsonKey("_id") id: Int,
    name: String = "test",
    meta: TestMeta,
    comment: Option[String],
    linkId: Option[Int],
    checks: Seq[TestCheck] = Seq()
) derives BsonEncoder,
      BsonDecoder

case class XXXCaseClass(
    a: Int,
    b: Int,
    c: Int,
    d: Int,
    e: Int,
    f: Int,
    g: Int,
    h: Int,
    i: Int,
    j: Int,
    k: Int,
    l: Int,
    m: Int,
    n: Int,
    o: Int,
    p: Int,
    q: Int,
    r: Int,
    s: Int,
    t: Int,
    u: Int,
    v: Int,
    w: Int,
    x: Int,
    y: Int,
    z: Int
) derives BsonEncoder,
      BsonDecoder
