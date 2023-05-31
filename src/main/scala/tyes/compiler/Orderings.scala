package tyes.compiler

import tyes.model.Term
import tyes.model.Type

object Orderings:

  given typeCompilerOrdering: Ordering[Type] = Ordering.by({
    case Type.Named(name) => (0, name)
    case Type.Variable(_) => (0, "")
    case Type.Composite(name, args*) => (args.length, name)
  })

  given termCompilerOrdering: Ordering[Term] = Ordering.by({
    case Term.Function(name, args*) => (args.length, name)
    case _ => (0, "")
  })
