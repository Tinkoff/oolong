package ru.tinkoff.oolong

import scala.quoted.*

import ru.tinkoff.oolong.Utils.AsIterable

object Meta:

  inline def meta[A](inline field: (A => (Any, String))*): Map[String, String] = ${ metaImpl[A]('field) }

  def metaImpl[A: Type](exp: Expr[Seq[(A => (Any, String))]])(using q: Quotes): Expr[Map[String, String]] =
    import q.reflect.*
    println(exp.asTerm.show(using Printer.TreeStructure))
    val exprList: List[Expr[(A => (Any, String))]] = exp match
      case AsIterable(list) => list.toList
    val res = exprList.map { expr =>
      expr match
        case '{ type t; (${ f }: (A => Tuple2[`t`, String])) } =>
          println(TypeRepr.of[t].typeSymbol.caseFields); println("Yes")
        case _ => println("NO")
      expr.asTerm.underlyingArgument.asExprOf[A => (Any, String)] match
        case Utils.AsTerm(
              Block(
                List(
                  DefDef(
                    "$anonfun",
                    _,
                    _,
                    Some(
                      Apply(
                        TypeApply(Select(Apply(_, List(Select(_, old))), "->"), _),
                        List(Literal(StringConstant(newName)))
                      )
                    )
                  )
                ),
                _
              )
            ) =>
          println(old -> newName)
          Expr(old    -> newName)
    }
//    val res = exp.asTerm.underlyingArgument.asExprOf[A => (Any, String)] match
//      case Utils.AsTerm(
//            Block(
//              List(
//                DefDef(
//                  "$anonfun",
//                  _,
//                  _,
//                  Some(
//                    Apply(
//                      TypeApply(Select(Apply(_, List(Select(_, old))), "->"), _),
//                      List(Literal(StringConstant(newName)))
//                    )
//                  )
//                )
//              ),
//              _
//            )
//          ) =>
//        Expr(old -> newName)
//    println(exp.asTerm.show(using Printer.TreeStructure))
    '{ Map.from(${ Expr.ofList(res.toList) }) }

end Meta
