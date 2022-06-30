package ru.tinkoff.oolong

import scala.quoted.*

import ru.tinkoff.oolong.Utils.AsIterable

case class QueryMeta[T](fun: Map[String, String])

object Meta:

  inline def meta[A](inline field: (A => (Any, String))*): QueryMeta[A] = ${ metaImpl[A]('field) }

  def metaImpl[A: Type](exp: Expr[Seq[(A => (Any, String))]])(using q: Quotes): Expr[QueryMeta[A]] =
    import q.reflect.*

    val typeRepr               = TypeRepr.of[A]
    val caseFieldsOfCaseFields = typeRepr.typeSymbol.caseFields.map(s => typeRepr.memberType(s).typeSymbol.caseFields)
    val caseFieldsTypes        = typeRepr.typeSymbol.caseFields.map(s => typeRepr.memberType(s).asType)

    val metaList: List[Expr[Map[String, String]]] = caseFieldsTypes.map { s =>
      s match
        case '[t] =>
          Expr.summon[QueryMeta[t]] match
            case Some('{ ${ expr0 }: QueryMeta[t] }) => Some('{ ${ expr0 }.fun })
            case None                                => None
    }.flatten

    println(caseFieldsOfCaseFields)
    println(caseFieldsOfCaseFields.map(s => s.map(_.flags.show)))
    println(caseFieldsTypes)

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
    '{ QueryMeta[A].apply(Map.from(${ Expr.ofList(res.toList) })) }

end Meta
