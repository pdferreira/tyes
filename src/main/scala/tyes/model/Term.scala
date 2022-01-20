package tyes.model

enum Term:
  case Constant[T](value: T)
  case Variable(name: String)
  case Function(name: String, args: Term*)

  def matches(otherTerm: Term): Option[Map[String, Term]] = (this, otherTerm) match {
    case (Term.Variable(name), _) => Some(Map(name -> otherTerm))
    case (Term.Function(name, args*), Term.Function(otherName, otherArgs*)) =>
      if name == otherName && args.length == otherArgs.length then
        args.zip(otherArgs).foldLeft(Option(Map[String, Term]())) {
          case (None, _) => None
          case (Some(prevSubst), (arg, otherArg)) => arg.matches(otherArg) flatMap { subst =>
            // if some variables had already been matched, check if they were matched with the same value
            val commonKeys = subst.keySet.intersect(prevSubst.keySet)
            if commonKeys.isEmpty || commonKeys.forall(k => subst(k) == prevSubst(k)) then
              Some(subst ++ prevSubst)
            else
              None
          }
        }
      else
        None
    case (Term.Constant(v1), Term.Constant(v2)) if v1 == v2 => Some(Map())
    case _ => None
  }