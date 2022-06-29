package ru.tinkoff.oolong

import scala.quoted.*

private[oolong] object Utils {

  def useWithinMacro(name: String) =
    scala.sys.error(s"`$name` should only be used within `compile` macro")

  object AsIterable {
    def unapply[T: Type](using q: Quotes)(
        expr: Expr[Iterable[T]]
    ): Option[Iterable[Expr[T]]] = {
      import q.reflect.*
      def rec(tree: Term): Option[Iterable[Expr[T]]] = tree match {
        case Repeated(elems, _) => Some(elems.map(x => x.asExprOf[T]))
        case Typed(e, _)        => rec(e)
        case Block(Nil, e)      => rec(e)
        case Apply(_, List(e))  => rec(e)
        case Inlined(_, Nil, e) => rec(e)
        case _                  => None
      }
      rec(expr.asTerm)
    }
  }

  object AnonfunBlock {
    def unapply(using quotes: Quotes)(
        term: quotes.reflect.Term
    ): Option[(String, quotes.reflect.Term)] = {
      import quotes.reflect.*
      term match {
        case Lambda(ValDef(paramName, _, _) :: Nil, rhs) =>
          Some((paramName, rhs))
        case _ => None
      }
    }
  }

  object InlinedSubquery {
    def unapply(using quotes: Quotes)(
        term: quotes.reflect.Term
    ): Option[quotes.reflect.Term] = {
      import quotes.reflect.*
      term match {
        case Inlined(_, _, expansion) => unapply(expansion)
        case Typed(term, _)           => Some(term)
        case _                        => None
      }
    }

    def unapply(expr: Expr[Any])(using quotes: Quotes): Option[quotes.reflect.Term] = {
      import quotes.reflect.*
      unapply(expr.asTerm)
    }
  }

  object AsTerm {
    def unapply(using quotes: Quotes)(
        expr: Expr[Any]
    ): Option[quotes.reflect.Term] = {
      import quotes.reflect.*
      Some(expr.asTerm)
    }
  }

  object PropSelector {
    private def parse(using quotes: Quotes)(
        term: quotes.reflect.Term
    ): Option[(String, List[String])] = {
      import quotes.reflect.*

      def loop(current: Term, acc: List[String]): Option[(String, List[String])] =
        current match {
          case Select(next, field) =>
            loop(next, field :: acc)
          case Apply(TypeApply(Ident("!!"), _), List(next)) =>
            loop(next, acc)
          case Ident(name) =>
            Some((name, acc))

          // Ident() can be inlined if queries are composed via "inline def"
          case Inlined(_, _, expansion) =>
            loop(expansion, acc)

          case _ =>
            None
        }

      loop(term, Nil)
    }

    def unapply(using quotes: Quotes)(
        term: quotes.reflect.Term
    ): Option[(String, List[String])] = parse(term)

    def unapply(using quotes: Quotes)(
        expr: Expr[_]
    ): Option[(String, List[String])] = {
      import quotes.reflect.*
      parse(expr.asTerm)
    }
  }
}
