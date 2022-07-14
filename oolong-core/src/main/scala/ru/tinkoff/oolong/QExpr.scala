package ru.tinkoff.oolong

import scala.collection.immutable.Stream.Cons
import scala.compiletime.asMatchable
import scala.quoted.Expr
import scala.util.Try

import ru.tinkoff.oolong.QExpr

sealed private[oolong] trait QExpr

private[oolong] object QExpr {

  case class Gte(x: QExpr, y: QExpr) extends QExpr

  case class Lte(x: QExpr, y: QExpr) extends QExpr

  case class Gt(x: QExpr, y: QExpr) extends QExpr

  case class Lt(x: QExpr, y: QExpr) extends QExpr

  case class Eq(x: QExpr, y: QExpr) extends QExpr

  case class Ne(x: QExpr, y: QExpr) extends QExpr

  case class Not(x: QExpr) extends QExpr

  case class In(x: QExpr, y: List[QExpr] | QExpr) extends QExpr
  case class Nin(x: QExpr, y: List[QExpr] | QExpr) extends QExpr

  case class And(children: List[QExpr]) extends QExpr

  case class Or(children: List[QExpr]) extends QExpr

  case class Prop(path: List[String]) extends QExpr

  case class Constant[T](s: T) extends QExpr

  case class ScalaCode(code: Expr[Any]) extends QExpr

  case class ScalaCodeIterable(code: Expr[Iterable[Any]]) extends QExpr

  case class Subquery(code: Expr[Any]) extends QExpr

  case class Exists(x: QExpr, y: QExpr) extends QExpr

  case class Size(x: QExpr, y: QExpr) extends QExpr

  object NumericConstant {

    /**
     * Here we guarantee that Constant, Numeric & Class are consistent
     */
    def unapply(qe: QExpr): Option[(Constant[_], Numeric[_], Class[_])] =
      qe match {
        case bi @ Constant(_: Byte)        => Some(bi, Numeric[Byte], classOf[Byte])
        case ci @ Constant(_: Int)         => Some(ci, Numeric[Int], classOf[Int])
        case li @ Constant(_: Long)        => Some(li, Numeric[Long], classOf[Long])
        case fi @ Constant(_: Float)       => Some(fi, Numeric[Float], classOf[Float])
        case di @ Constant(_: Double)      => Some(di, Numeric[Double], classOf[Double])
        case bii @ Constant(_: BigInt)     => Some(bii, Numeric[BigInt], classOf[BigInt])
        case bdi @ Constant(_: BigDecimal) => Some(bdi, Numeric[BigDecimal], classOf[BigDecimal])
        case _                             => None
      }
  }
}
