package tyes.model.terms

trait TermOps[TTerm <: TermOps[TTerm, TConstant], TConstant](builder: TermBuilder[TTerm, TConstant]):
  this: TTerm =>

  private object Variable:
    def apply(name: String): TTerm & TermVariable = builder.applyVariable(name)
    def unapply(term: TTerm): Option[String] = builder.unapplyVariable(term)

  private object Constant:
    def apply(value: TConstant): TTerm = builder.applyConstant(value) 
    def unapply(term: TTerm): Option[TConstant] = builder.unapplyConstant(term)

  private object Function:
    def apply(name: String, args: TTerm*): TTerm = builder.applyFunction(name, args*)
    def unapplySeq(term: TTerm): Option[(String, Seq[TTerm])] = builder.unapplyFunction(term)

  def matches(otherTerm: TTerm): Option[Map[String, TTerm]] = (this, otherTerm) match {
    case (Variable(name), _) => Some(Map(name -> otherTerm))
    case (Function(name, args*), Function(otherName, otherArgs*)) =>
      if name == otherName && args.length == otherArgs.length then
        args.zip(otherArgs).foldLeft(Option(Map[String, TTerm]())) {
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
    case (Constant(v1), Constant(v2)) if v1 == v2 => Some(Map())
    case _ => None
  }

  def variables: Set[String] = this match {
    case Variable(name) => Set(name)
    case Constant(_) => Set()
    case Function(_, args*) => Set.concat(args.map(t => t.variables)*)
  }

  def substitute(subst: Map[String, TTerm]): TTerm = this match {
    case Variable(name) =>
      if subst.contains(name) then
        subst(name)
      else
        this
    case Constant(_) => this
    case Function(name, args*) => Function(name, args.map(_.substitute(subst))*)
  }

  def unifies(otherTerm: TTerm): Option[Map[String, TTerm]] = (this, otherTerm) match {
    case (Variable(name), Variable(otherName)) if name == otherName => Some(Map())
    case (Variable(name), _) =>
      if otherTerm.variables.contains(name) then
        None
      else
        Some(Map(name -> otherTerm))
    case (_, Variable(otherName)) =>
      if this.variables.contains(otherName) then
        None
      else
        Some(Map(otherName -> this))
    case (Constant(value), Constant(otherValue)) if value == otherValue => Some(Map())
    case (Function(name, args*), Function(otherName, otherArgs*)) =>
      if name == otherName && args.length == otherArgs.length then
        args.zip(otherArgs).foldLeft(Option(Map[String, TTerm]())) {
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

  def overlaps(otherTerm: TTerm): Boolean =
    val commonVarNames = this.variables.intersect(otherTerm.variables)
    val freshNamesSubst = Map.from(commonVarNames.map(name => name -> Variable("$" + name)))
    val freshOtherTerm = otherTerm.substitute(freshNamesSubst)
    this.unifies(freshOtherTerm).isDefined

  def isGround: Boolean = this.variables.isEmpty

  override def toString(): String = this match {
    case Constant(value) => s"C:$value"
    case Variable(name) =>  s"V:$name"
    case Function(name, args*) => args.mkString(s"$name(", ", ", ")")
  }
