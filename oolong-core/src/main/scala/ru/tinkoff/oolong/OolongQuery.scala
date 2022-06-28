package ru.tinkoff.oolong

import scala.quoted.{Expr, Quotes, ToExpr}
import ru.tinkoff.oolong.OolongQuery
import ru.tinkoff.oolong.OolongQuery.*

sealed trait OolongQuery

object OolongQuery {

  case class Gte(x: OolongQuery, y: OolongQuery) extends OolongQuery

  case class Lte(x: OolongQuery, y: OolongQuery) extends OolongQuery

  case class Gt(x: OolongQuery, y: OolongQuery) extends OolongQuery

  case class Lt(x: OolongQuery, y: OolongQuery) extends OolongQuery

  case class Eq(x: OolongQuery, y: OolongQuery) extends OolongQuery

  case class Ne(x: OolongQuery, y: OolongQuery) extends OolongQuery

  case class Not(x: OolongQuery) extends OolongQuery

  case class In(x: OolongQuery, y: List[OolongQuery] | OolongQuery) extends OolongQuery
  case class Nin(x: OolongQuery, y: List[OolongQuery] | OolongQuery) extends OolongQuery

  case class And(children: List[OolongQuery]) extends OolongQuery

  case class Or(children: List[OolongQuery]) extends OolongQuery

  case class Plus(x: OolongQuery, y: OolongQuery) extends OolongQuery

  case class Minus(x: OolongQuery, y: OolongQuery) extends OolongQuery

  case class Prop(path: List[String]) extends OolongQuery

  case class Constant[T](s: T) extends OolongQuery

  case class ScalaCode(code: Expr[Any]) extends OolongQuery

  case class ScalaCodeIterable(code: Expr[Iterable[Any]]) extends OolongQuery

  case class Subquery(code: Expr[Any]) extends OolongQuery

  case class Exists(x: OolongQuery, y: OolongQuery) extends OolongQuery

  case class Size(x: OolongQuery, y: OolongQuery) extends OolongQuery

  private[oolong] object Lift:

    given ToExpr[Prop] with
      def apply(x: Prop)(using Quotes): Expr[Prop] = '{ Prop( ${ Expr(x.path) } ) }

    def apply(node: OolongQuery)(using Quotes): Expr[OolongQuery] = node match {
      case Eq(x, y) =>
        '{ Eq(
          ${ Lift(x) },
          ${ Lift(y) }
        ) }
      case prop: Prop => Expr(prop)
      case ScalaCode(code) => '{ ScalaCode('code) }
    }
}
