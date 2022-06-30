package ru.tinkoff.oolong.mongo

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

import ru.tinkoff.oolong.Utils.AsIterable
import ru.tinkoff.oolong.Utils.AsSome
import ru.tinkoff.oolong.Utils.AsTerm
import ru.tinkoff.oolong.bson.annotation.BsonKey

private[oolong] object RenderUtils:

  def renderCaseClass[A: Type](value: Expr[A])(using q: Quotes): String =
    import q.reflect.*

    def parseCaseClass[A: Type](value: Expr[A]): String =
      val repr = TypeRepr.of[A].typeSymbol
      val paramsAndDefsOpt = value.asTerm.underlyingArgument.asExprOf[A] match
        case AsTerm(
              Apply(
                Select(Select(This(Some(_)), _), "apply"),
                list
              )
            ) =>
          Some(list -> Map.empty)
        case AsTerm(
              Apply(
                Select(Ident(_), "apply"),
                list
              )
            ) =>
          Some(list -> Map.empty)
        case AsTerm(
              Block(
                valDefList,
                Apply(
                  Select(Select(This(Some(_)), _), "apply"),
                  list
                )
              )
            ) =>
          val definitions = valDefList.collect { case ValDef(name, _, Some(definition)) => name -> definition }.toMap
          Some(list -> definitions)
        case AsTerm(
              Block(
                valDefList,
                Apply(
                  Select(Ident(_), "apply"),
                  list
                )
              )
            ) =>
          val definitions = valDefList.collect { case ValDef(name, _, Some(definition)) => name -> definition }.toMap
          Some(list -> definitions)
        case _ => None

      paramsAndDefsOpt match
        case Some(params -> definitions) =>
          val renames = TypeRepr
            .of[A]
            .classSymbol
            .flatMap(
              _.primaryConstructor.paramSymss.headOption
                .map(
                  _.map(s => s.name -> getRenamedFieldName(s.getAnnotation(TypeRepr.of[BsonKey].typeSymbol)))
                )
            )
            .map(_.toMap[String, Option[String]])
            .map(_.collect { case (f, Some(s)) => (f, s) })
            .getOrElse(Map.empty)

          val fields = repr.caseFields.map(_.name).map(name => renames.getOrElse(name, name))
          val res = fields.zip(params).map { case (name, value) =>
            name + ":" + " " + parseConstant[A](value.asExpr)(definitions.map { case (a, b) => (a.toString, b) })
          }
          res.mkString("{", ", ", "}")
        case _ => "?"
    end parseCaseClass

    def getRenamedFieldName(annot: Option[Term]): Option[String] =
      annot match
        case Some(Apply(Select(New(TypeIdent("BsonKey")), _), List(Literal(StringConstant(renamed))))) => Some(renamed)
        case _                                                                                         => None
    end getRenamedFieldName

    def parseConstant[A: Type](expr: Expr[Any])(definitions: Map[String, q.reflect.Term]): String =
      expr match
        case RenderedLiteral(repr) => repr
        case AsTerm(Select(_, name)) if name.contains("$lessinit$greater$default$") =>
          TypeRepr
            .of[A]
            .typeSymbol
            .companionClass
            .declaredMethod(name)
            .headOption
            .flatMap(_.tree.asInstanceOf[DefDef].rhs)
            .map(s => parseConstant[A](s.asExpr)(definitions))
            .getOrElse(name)
        case AsTerm(NamedArg(_, const))        => parseConstant(const.asExpr)(definitions)
        case AsTerm(Select(Ident(cl), method)) => s"Function($cl.$method)"
        case AsTerm(Apply(Select(Ident(cl), method), list)) =>
          s"Function($cl.$method(${list.map(_.asExpr).map(parseConstant(_)(Map.empty)).mkString(", ")}))"
        case AsTerm(Ident(name)) =>
          if (name == "None") "null"
          else
            definitions
              .get(name)
              .map(s => parseConstant(s.asExpr)(definitions))
              .getOrElse(name)
        case AsIterable(list) => list.map(parseConstant(_)(definitions)).mkString("[", ", ", "]")
        case AsSome(value)    => parseConstant(value)(definitions)
        case '{ ${ x }: t } if TypeRepr.of[t].typeSymbol.flags.is(Flags.Case) => parseCaseClass[t](x)
        case _                                                                => "?"
    end parseConstant

    parseCaseClass(value)

  end renderCaseClass

  object RenderedLiteral:
    def unapply(expr: Expr[Any])(using q: Quotes): Option[String] =
      import q.reflect.*
      expr match
        case AsTerm(Literal(DoubleConstant(c)))  => Some(c.toString)
        case AsTerm(Literal(FloatConstant(c)))   => Some(c.toString)
        case AsTerm(Literal(LongConstant(c)))    => Some(c.toString)
        case AsTerm(Literal(IntConstant(c)))     => Some(c.toString)
        case AsTerm(Literal(ShortConstant(c)))   => Some(c.toString)
        case AsTerm(Literal(ByteConstant(c)))    => Some(c.toString)
        case AsTerm(Literal(StringConstant(c)))  => Some(s"\"$c\"")
        case AsTerm(Literal(CharConstant(c)))    => Some(c.toString)
        case AsTerm(Literal(BooleanConstant(c))) => Some(c.toString)
        case _                                   => None

  end RenderedLiteral

end RenderUtils
