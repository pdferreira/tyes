package tyes.compiler

import tyes.model.Label
import tyes.model.Term
import tyes.model.Type

object Orderings:

  given labelCompilerOrdering: Ordering[Label] = Ordering.by({
    case Label.Constant(name) => (0, name)
    case Label.Variable(name) => (0, "")
  })

  given typeCompilerOrdering: Ordering[Type] = Ordering.by({
    case Type.Named(name) => (0, name)
    case Type.Label(_) => (0, "")
    case Type.Variable(_) => (0, "")
    case Type.Composite(name, args*) => (args.length, name)
    case Type.Range(function, _, _,  argTemplates, _, _, _) => (argTemplates.size + 1, function)
  })

  given termCompilerOrdering: Ordering[Term] = Ordering.by({
    case Term.Function(name, args*) => (args.length, name)
    case Term.Range(function, _, _, argTemplates, _, _, _) => (argTemplates.size + 1, function)
    case _ => (0, "")
  })
