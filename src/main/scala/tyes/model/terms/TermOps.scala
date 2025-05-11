package tyes.model.terms

import tyes.model.indexes.*
import scala.reflect.ClassTag

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

  private object Range:
    def apply(
      function: String,
      cursor: String,
      template: TTerm,
      minIndex: Int,
      maxIndex: Either[String, Int],
      seed: Option[TTerm] = None
    ): TTerm = builder.applyRange(function, cursor, template, minIndex, maxIndex, seed)

    def unapply(term: TTerm): Option[(
      String,
      String,
      TTerm,
      Int,
      Either[String, Int],
      Option[TTerm]
    )] = builder.unapplyRange(term)

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
    case (Range(function, cursor, template, minIndex, maxIndex, seed), _) => (maxIndex, seed, otherTerm) match {
      case (Left(maxVar), _, Function(`function`, left, right)) =>
        val innerMaxVar = "$" + maxVar
        Range(function, cursor, template, minIndex, Left(innerMaxVar), seed)
          .matches(left)
          .flatMap { innerSubst =>
            val realMax = Constant.unapply(innerSubst(innerMaxVar)).get.asInstanceOf[Int] + 1
            val instance = template.replaceIndex(cursor, realMax.toString).substitute(innerSubst - innerMaxVar)
            val maxVarSubst = Map(maxVar -> Constant(realMax.asInstanceOf[TConstant]))
            val lSubst = (innerSubst - innerMaxVar) ++ maxVarSubst
            instance.matches(right).map(lSubst ++ _)
          }
          // otherwise, since max is unbounded, let's try to match the seed
          .orElse {
            for
              s <- seed
              subst <- s.matches(otherTerm)
            yield
              subst + (maxVar -> Constant((minIndex - 1).asInstanceOf[TConstant]))
          }
      case (Left(maxVar), None, _) =>
        template.replaceIndex(cursor, minIndex.toString).matches(otherTerm).map { subst =>
          subst + (maxVar -> Constant(minIndex.asInstanceOf[TConstant]))
        }
      case (Left(maxVar), Some(seed), _) =>
        seed.matches(otherTerm).map { subst =>
          subst + (maxVar -> Constant((minIndex - 1).asInstanceOf[TConstant]))
        }
      case (Right(maxIndexVal), _, Function(`function`, left, right)) if minIndex < maxIndexVal =>
        Range(function, cursor, template, minIndex, Right(maxIndexVal - 1), seed)
          .matches(left)
          .flatMap { innerSubst =>
            val instance = template.replaceIndex(cursor, maxIndexVal.toString).substitute(innerSubst)
            instance.matches(right).map(innerSubst ++ _)
          }
      case (Right(maxIndexVal), _, _) if minIndex == maxIndexVal =>
        val instance = template.replaceIndex(cursor, minIndex.toString)
        seed match {
          case Some(s) => Function(function, s, instance).matches(otherTerm)
          case None => instance.matches(otherTerm)
        }
      case (Right(_), Some(seed), _) => seed.matches(otherTerm)
      case (Right(_), None, _) => None 
    }
    case _ => None
  }

  def variables: Set[String] = this match {
    case Variable(name) => Set(name)
    case Constant(_) => Set()
    case Function(_, args*) => Set.concat(args.map(t => t.variables)*)
    case Range(_, cursor, template, minIndex, maxIndex, seed) =>
      val indexes = maxIndex match {
        case Right(i) => (minIndex to i).map(_.toString).toSet
        case Left(s) => Set(minIndex.toString, s)
      }
      val indexVars = maxIndex.left.toOption.toSet
      val seedVars = seed.map(_.variables).getOrElse(Set())
      val expandedVars = indexes.flatMap(i => template.replaceIndex(cursor, i).variables)
      indexVars ++ expandedVars ++ seedVars
  }

  def substitute(subst: Map[String, TTerm]): TTerm = this match {
    case Variable(name) =>
      if subst.contains(name) then
        subst(name)
      else
        this
    case Constant(_) => this
    case Function(name, args*) => Function(name, args.map(_.substitute(subst))*)
    case Range(function, cursor, template, minIndex, maxIndex, seed) =>
      val newMaxIndex = maxIndex match { // TODO: clean this up by defining a proper Index data type
        case Left(s) if subst.contains(s) => subst(s) match {
          case Constant(i: Int) => Right(i)
        }
        case _ => maxIndex
      }
      if newMaxIndex.isRight then
        val elems = (minIndex to newMaxIndex.right.get).map(i => template.replaceIndex(cursor, i.toString).substitute(subst))
        val allElems = seed.map(_.substitute(subst) +: elems).getOrElse(elems)
        allElems.drop(1).foldLeft(allElems.head) { (acc, elem) => Function(function, acc, elem) }
      else
        val newTemplate = template.substitute(subst - cursor)
        val newSeed = seed.map(_.substitute(subst))
        Range(function, cursor, newTemplate, minIndex, newMaxIndex, newSeed)
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
    case (
      Range(function, cursor, template, minIndex, maxIndex, seed),
      Range(otherFunction, otherCursor, otherTemplate, otherMinIndex, otherMaxIndex, otherSeed)
    ) =>
      if function == otherFunction && minIndex == otherMinIndex then
        for
          maxIdxSubst <- (maxIndex, otherMaxIndex) match {
            case (Left(s), Left(otherS)) if s == otherS => Some(Map())
            case (Left(s), Left(otherS)) => Some(Map(s -> Variable(otherS)))
            case (Left(s), Right(i)) => Some(Map(s -> Constant(i.asInstanceOf[TConstant])))
            case (Right(i), Right(otherI)) if i == otherI => Some(Map())
            case _ => None
          }
          seedSubst <- (seed, otherSeed) match {
            case (None, None) => Some(Map())
            case (Some(s), Some(otherS)) => s.substitute(maxIdxSubst).unifies(otherS.substitute(maxIdxSubst))
            case _ => None
          }
          templateSubst <- template.substitute(seedSubst).unifies(otherTemplate.replaceIndex(otherCursor, cursor).substitute(seedSubst))
        yield
          val seedSubst2 = seedSubst.map { case (k, v) => (k, v.substitute(templateSubst)) }
          val maxIdxSubst2 = maxIdxSubst.map { case (k, v) => (k, v.substitute(seedSubst2)) }
          seedSubst2 ++ maxIdxSubst2 ++ templateSubst
      else
        None
    case _ => None
  }

  def overlaps(otherTerm: TTerm): Boolean =
    val commonVarNames = this.variables.intersect(otherTerm.variables)
    val freshNamesSubst = Map.from(commonVarNames.map(name => name -> Variable('$' + name)))
    val freshOtherTerm = otherTerm.substitute(freshNamesSubst)
    this.unifies(freshOtherTerm).isDefined

  def isGround: Boolean = this.variables.isEmpty

  def replaceIndex(oldIdxStr: String, newIdxStr: String): TTerm = this match {
    case Constant(_) => this
    case Variable(rawName) => extractIndex(rawName) match {
      case Some((name, `oldIdxStr`)) => Variable(indexedVar(name, newIdxStr))
      case _ => this
    }
    case Function(name, args*) => Function(name, args.map(_.replaceIndex(oldIdxStr, newIdxStr))*)
    case Range(function, cursor, template, minIndex, maxIndex, seed) =>
      val newTemplate = if cursor != oldIdxStr then template.replaceIndex(oldIdxStr, newIdxStr) else template
      val newSeed = seed.map(_.replaceIndex(oldIdxStr, newIdxStr))
      val newMaxIndex = maxIndex match {
        case Left(s) if s == oldIdxStr => oldIdxStr.toIntOption.toRight(newIdxStr)
        case _ => maxIndex
      }
      Range(function, cursor, newTemplate, minIndex, newMaxIndex, newSeed)
  }

  override def toString(): String = this match {
    case Constant(value) => s"C:$value"
    case Variable(name) =>  s"V:$name"
    case Function(name, args*) => args.mkString(s"$name(", ", ", ")")
    case Range(function, cursor, template, minIndex, maxIndex, seed) =>
      val seedStr = seed.map(_.toString + ", ").getOrElse("")
      val maxIndexStr = maxIndex.fold(s => s, i => i.toString)
      s"R:${function}[$cursor in $minIndex..$maxIndexStr]($seedStr$template)"
  }
