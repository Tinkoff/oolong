package ru.tinkoff.oolong

private[oolong] object LogicalOptimizer {

  def optimize(ast: OolongQuery): OolongQuery = {

    // Example:
    //
    //         grandparent: And(And(x, y), z))
    //              /            \
    //    parent: And(x, y)      z
    //    /              \
    //   x                y
    //
    // should be transformed into
    //
    // grandparent: And(x, y, z)
    //     /      |      \
    //    x       y       z
    def flatten(grandparent: OolongQuery): OolongQuery = grandparent match {
      case OolongQuery.And(parents) =>
        val newParents = parents.flatMap {
          case OolongQuery.And(children) => children
          case parent              => List(parent)
        }
        OolongQuery.And(newParents)
      case OolongQuery.Or(parents) =>
        val newParents = parents.flatMap {
          case OolongQuery.Or(children) => children
          case parent             => List(parent)
        }
        OolongQuery.Or(newParents)
      case _ =>
        grandparent
    }

    ast match {
      case OolongQuery.And(children)     => flatten(OolongQuery.And(children.map(optimize)))
      case OolongQuery.Or(children)      => flatten(OolongQuery.Or(children.map(optimize)))
      case OolongQuery.Gte(x, y)         => OolongQuery.Gte(optimize(x), optimize(y))
      case OolongQuery.Lte(x, y)         => OolongQuery.Lte(optimize(x), optimize(y))
      case OolongQuery.Eq(x, y)          => OolongQuery.Eq(optimize(x), optimize(y))
      case OolongQuery.Plus(x, y)        => OolongQuery.Plus(optimize(x), optimize(y))
      case OolongQuery.Minus(x, y)       => OolongQuery.Minus(optimize(x), optimize(y))
      case OolongQuery.Not(OolongQuery.Not(e)) => e
      case _                       => ast
    }
  }
}
