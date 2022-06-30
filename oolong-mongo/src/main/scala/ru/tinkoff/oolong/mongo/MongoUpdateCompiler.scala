package ru.tinkoff.oolong.mongo

import scala.quoted.*

import org.mongodb.scala.bson.BsonBoolean
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.BsonDouble
import org.mongodb.scala.bson.BsonInt32
import org.mongodb.scala.bson.BsonInt64
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.bson.BsonValue

import ru.tinkoff.oolong.*
import ru.tinkoff.oolong.UExpr.FieldUpdateExpr
import ru.tinkoff.oolong.mongo.MongoUpdateNode as MU

object MongoUpdateCompiler extends Backend[UExpr, MU, BsonDocument] {

  def opt[T: Type](ast: UExpr)(using quotes: Quotes): MU = {
    import quotes.reflect.*

    ast match {
      case UExpr.Update(ops) =>
        MU.Update(ops.map {
          case FieldUpdateExpr.Set(prop, expr)         => MU.MongoUpdateOp.Set(MU.Prop(prop.path), opt(expr))
          case FieldUpdateExpr.Inc(prop, expr)         => MU.MongoUpdateOp.Inc(MU.Prop(prop.path), opt(expr))
          case FieldUpdateExpr.Max(prop, expr)         => MU.MongoUpdateOp.Max(MU.Prop(prop.path), opt(expr))
          case FieldUpdateExpr.Min(prop, expr)         => MU.MongoUpdateOp.Min(MU.Prop(prop.path), opt(expr))
          case FieldUpdateExpr.Mul(prop, expr)         => MU.MongoUpdateOp.Mul(MU.Prop(prop.path), opt(expr))
          case FieldUpdateExpr.Rename(prop, expr)      => MU.MongoUpdateOp.Rename(MU.Prop(prop.path), opt(expr))
          case FieldUpdateExpr.SetOnInsert(prop, expr) => MU.MongoUpdateOp.SetOnInsert(MU.Prop(prop.path), opt(expr))
          case FieldUpdateExpr.Unset(prop)             => MU.MongoUpdateOp.Unset(MU.Prop(prop.path))
        })
      case UExpr.ScalaCode(code) => MU.ScalaCode(code)
      case UExpr.Constant(t)     => MU.Constant(t)
      case _                     => report.errorAndAbort("Unexpected expr " + pprint(ast))
    }
  }

  def render(query: MU)(using quotes: Quotes): String = query match {
    case MU.Update(ops) =>
      List(
        renderOps(
          ops.collect { case s: MU.MongoUpdateOp.Set => s }.map(op => render(op.prop) + ": " + render(op.value))
        )("$set"),
        renderOps(
          ops.collect { case s: MU.MongoUpdateOp.Inc => s }.map(op => render(op.prop) + ": " + render(op.value))
        )("$inc"),
        renderOps(ops.collect { case s: MU.MongoUpdateOp.Unset => s }.map(op => render(op.prop) + ": " + "\"\""))(
          "$unset"
        ),
        renderOps(
          ops.collect { case s: MU.MongoUpdateOp.Max => s }.map(op => render(op.prop) + ": " + render(op.value))
        )("$max"),
        renderOps(
          ops.collect { case s: MU.MongoUpdateOp.Min => s }.map(op => render(op.prop) + ": " + render(op.value))
        )("$min"),
        renderOps(
          ops.collect { case s: MU.MongoUpdateOp.Mul => s }.map(op => render(op.prop) + ": " + render(op.value))
        )("$mul"),
        renderOps(
          ops.collect { case s: MU.MongoUpdateOp.Rename => s }.map(op => render(op.prop) + ": " + render(op.value))
        )("$rename"),
        renderOps(
          ops.collect { case s: MU.MongoUpdateOp.SetOnInsert => s }.map(op => render(op.prop) + ": " + render(op.value))
        )("$setOnInsert")
      ).flatten
        .mkString("{\n", ",\n", "\n}")

    case MU.Prop(path) =>
      "\"" + path.mkString(".") + "\""

    case MU.Constant(s: String) =>
      "\"" + s + "\""

    case MU.Constant(s: Any) =>
      s.toString // also limit

    case MU.ScalaCode(expr) =>
      expr match
        case '{ ${ x }: t } => RenderUtils.renderCaseClass[t](x)
        case _              => "?"

  }

  def renderOps(ops: List[String])(op: String) =
    ops match
      case Nil  => None
      case list => Some(s"\t $op: { " + list.mkString(", ") + " }")

  def target(optRepr: MU)(using quotes: Quotes): Expr[BsonDocument] = {
    import quotes.reflect.*

    def targetOps(setters: List[MU.MongoUpdateOp]): List[Expr[(String, BsonValue)]] =
      setters.map { case op: MU.MongoUpdateOp =>
        val key       = op.prop.path.mkString(".")
        val valueExpr = handleValues(op.value)
        '{ ${ Expr(key) } -> $valueExpr }
      }

    optRepr match {
      case MU.Update(ops) =>
        val tSetters      = targetOps(ops.collect { case s: MU.MongoUpdateOp.Set => s })
        val tUnsets       = targetOps(ops.collect { case s: MU.MongoUpdateOp.Unset => s })
        val tIncs         = targetOps(ops.collect { case s: MU.MongoUpdateOp.Inc => s })
        val tMaxs         = targetOps(ops.collect { case s: MU.MongoUpdateOp.Max => s })
        val tMins         = targetOps(ops.collect { case s: MU.MongoUpdateOp.Min => s })
        val tMuls         = targetOps(ops.collect { case s: MU.MongoUpdateOp.Mul => s })
        val tRenames      = targetOps(ops.collect { case s: MU.MongoUpdateOp.Rename => s })
        val tSetOnInserts = targetOps(ops.collect { case s: MU.MongoUpdateOp.SetOnInsert => s })

        // format: off
        def updaterGroup(groupName: String, updaters: List[Expr[(String, BsonValue)]]): Option[Expr[(String, BsonDocument)]] =
          if (updaters.isEmpty)
            None
          else
            Some('{
              ${ Expr(groupName) } -> BsonDocument(${ Expr.ofList(updaters)} )
            })

        val updateList: List[Expr[(String, BsonDocument)]] = List(
          updaterGroup("$set", tSetters),
          updaterGroup("$unset", tUnsets),
          updaterGroup("$inc", tIncs),
          updaterGroup("$max", tMaxs),
          updaterGroup("$min", tMins),
          updaterGroup("$mul", tMuls),
          updaterGroup("$rename", tRenames),
          updaterGroup("$setOnInsert", tSetOnInserts),
        ).flatten

        '{
          BsonDocument(
            ${ Expr.ofList(updateList) }
          )
        }
        //format: on
      case _ => report.errorAndAbort(s"Unexpected expr " + pprint(optRepr))
    }
  }

  def handleValues(expr: MongoUpdateNode)(using q: Quotes): Expr[BsonValue] =
    import q.reflect.*
    expr match {
      case MU.Constant(i: Long) =>
        '{ BsonInt64.apply(${ Expr(i: Long) }) }
      case MU.Constant(i: Int) =>
        '{ BsonInt32.apply(${ Expr(i: Int) }) }
      case MU.Constant(i: Short) =>
        '{ BsonInt32.apply(${ Expr(i: Short) }) }
      case MU.Constant(i: Byte) =>
        '{ BsonInt32.apply(${ Expr(i: Byte) }) }
      case MU.Constant(s: Double) =>
        '{ BsonDouble.apply(${ Expr(s: Double) }) }
      case MU.Constant(s: Float) =>
        '{ BsonDouble.apply(${ Expr(s: Float) }) }
      case MU.Constant(s: String) =>
        '{ BsonString.apply(${ Expr(s: String) }) }
      case MU.Constant(s: Char) =>
        '{ BsonString.apply(${ Expr((s: Char).toString) }) }
      case MU.Constant(b: Boolean) =>
        '{ BsonBoolean.apply(${ Expr(b: Boolean) }) }
      case MU.ScalaCode(code) => BsonUtils.extractLifted(code)
      case _                  => report.errorAndAbort(s"Given type is not literal constant")
    }
}
