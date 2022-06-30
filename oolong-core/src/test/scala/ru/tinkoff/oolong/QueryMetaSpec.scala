package ru.tinkoff.oolong

import org.scalatest.funsuite.AnyFunSuite

class QueryMetaSpec extends AnyFunSuite {

  case class LowLevel(fieldOne: String)
  case class MiddleLevel(fieldOne: LowLevel)
  case class UpperLevel(fieldOne: Int, fieldTwo: String, fieldThree: MiddleLevel)

  test("QueryMeta[UpperLevel] is correct") {
    inline given QueryMeta[LowLevel] = queryMeta(_.fieldOne -> "fieldOneRenamed")
    inline given QueryMeta[MiddleLevel] = queryMeta(_.fieldOne -> "fieldTwo")
    val meta: QueryMeta[UpperLevel] = queryMeta(_.fieldTwo -> "fieldTwoRenamed", _.fieldThree -> "fieldThreeRenamed")

    assert(
      meta == QueryMeta[UpperLevel](
        Map(
          "fieldTwo"                     -> "fieldTwoRenamed",
          "fieldThree"                   -> "fieldThreeRenamed",
          "fieldThree.fieldOne"          -> "fieldThreeRenamed.fieldTwo",
          "fieldThree.fieldOne.fieldOne" -> "fieldThreeRenamed.fieldTwo.fieldOneRenamed",
        )
      )
    )
  }

}
