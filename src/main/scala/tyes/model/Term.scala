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

  def variables: Set[String] = this match {
    case Term.Variable(name) => Set(name)
    case Term.Constant(_) => Set()
    case Term.Function(_, args*) => Set.concat(args.map(t => t.variables)*)
  }

  def substitute(subst: Map[String, Term]): Term = this match {
    case Term.Variable(name) =>
      if subst.contains(name) then
        subst(name)
      else
        this
    case Term.Constant(_) => this
    case Term.Function(name, args*) => Function(name, args.map(_.substitute(subst))*)
  }

  def unifies(otherTerm: Term): Option[Map[String, Term]] = (this, otherTerm) match {
    case (Term.Variable(name), Term.Variable(otherName)) if name == otherName => Some(Map())
    case (Term.Variable(name), _) =>
      if otherTerm.variables.contains(name) then
        None
      else
        Some(Map(name -> otherTerm))
    case (_, Term.Variable(otherName)) =>
      if this.variables.contains(otherName) then
        None
      else
        Some(Map(otherName -> this))
    case (Term.Constant(value), Term.Constant(otherValue)) if value == otherValue => Some(Map())
    case (Term.Function(name, args*), Term.Function(otherName, otherArgs*)) =>
      if name == otherName && args.length == otherArgs.length then
        args.zip(otherArgs).foldLeft(Option(Map[String, Term]())) {
          case (None, _) => None
          case (Some(prevSubst), (arg, otherArg)) => 
            val substArg = arg.substitute(prevSubst)
            val substOtherArg = otherArg.substitute(prevSubst)
            substArg.unifies(substOtherArg).map { subst =>
              prevSubst.map({ case (k, v) => (k, v.substitute(subst)) }) ++ subst
            }
        }
      else
        None
    case _ => None
  }

  def overlaps(otherTerm: Term): Boolean =
    val commonVarNames = this.variables.intersect(otherTerm.variables)
    val freshNamesSubst = Map.from(commonVarNames.map(name => name -> Term.Variable('$' + name)))
    val freshOtherTerm = otherTerm.substitute(freshNamesSubst)
    this.unifies(freshOtherTerm).isDefined
