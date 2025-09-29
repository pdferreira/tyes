package tyes.model.terms

import scala.reflect.ClassTag
import tyes.model.indexes.*
import tyes.model.ranges.*

trait TermOps[TTerm <: TermOps[TTerm, TConstant], TConstant](builder: TermBuilder[TTerm, TConstant]):
  this: TTerm =>

  private object Variable:
    def apply(name: String): TTerm & TermVariable = builder.applyVariable(name)
    def unapply(term: TTerm & TermVariable): Option[String] = builder.unapplyVariable(term)

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
      holeArgIdx: Int,
      argTemplates: Seq[TTerm],
      minIndex: Int,
      maxIndex: Index,
      holeIsMax: Boolean,
      holeSeed: Option[TTerm] = None
    ): TTerm & TermRange[TTerm] = builder.applyRange(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed)

    def unapply(term: TTerm): Option[(
      String,
      String,
      Int,
      Seq[TTerm],
      Int,
      Index,
      Boolean,
      Option[TTerm]
    )] = builder.unapplyRange(term)

  def matches(otherTerm: TTerm): Option[Map[String, TTerm]] = (this, otherTerm) match {
    case (Constant(v1), Constant(v2)) if v1 == v2 => Some(Map())
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
    case (Function(name, args*), Range(function, cursor, holeArgIdx, argTemplates, minIndex, Index.Number(maxIndex), /*holeIsMax*/false, holeSeed))
      if name == function && minIndex < maxIndex
    =>
      args(holeArgIdx)
        .matches(Range(function, cursor, holeArgIdx, argTemplates, minIndex, Index.Number(maxIndex - 1), false, holeSeed))
        .flatMap { holeSubst =>
          val remainingArgs = args.take(holeArgIdx) ++ args.drop(holeArgIdx + 1)
          remainingArgs.zip(argTemplates).foldLeft(Option(holeSubst)) {
            case (None, _) => None
            case (Some(accSubst), (remainingArg, template)) =>
              val instance = template.replaceIndex(cursor, maxIndex.toString).substitute(accSubst)
              remainingArg.matches(instance).map(accSubst ++ _)
          }
        }
    case (Function(name, args*), Range(function, cursor, holeArgIdx, argTemplates, minIndex, Index.Number(maxIndex), /*holeIsMax*/true, holeSeed))
      if name == function && minIndex < maxIndex
    =>
      val remainingArgs = args.take(holeArgIdx) ++ args.drop(holeArgIdx + 1)
      remainingArgs.zip(argTemplates)
        .foldLeft(Option(Map[String, TTerm]())) {
          case (None, _) => None
          case (Some(accSubst), (remainingArg, template)) =>
            val instance = template.replaceIndex(cursor, maxIndex.toString).substitute(accSubst)
            remainingArg.matches(instance).map(accSubst ++ _)
        }
        .flatMap { rSubst =>
          args(holeArgIdx)
            .matches(Range(function, cursor, holeArgIdx, argTemplates, minIndex + 1, Index.Number(maxIndex), true, holeSeed))
        }
    case (Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed), _) => (maxIndex, holeIsMax, holeSeed, otherTerm) match {
      case (Index.Variable(maxVar, minOccurs), /*holeIsMax*/false, _, Function(`function`, args*)) =>
        val innerMaxVar = "$" + maxVar
        Range(function, cursor, holeArgIdx, argTemplates, minIndex, Index.Variable(innerMaxVar, minOccurs - 1), false, holeSeed)
          .matches(args(holeArgIdx))
          .flatMap { holeSubst =>
            val realMax = Constant.unapply(holeSubst(innerMaxVar)).get.asInstanceOf[Int] + 1
            val maxVarSubst = Map(maxVar -> Constant(realMax.asInstanceOf[TConstant]))
            val hSubst = (holeSubst - innerMaxVar) ++ maxVarSubst
            val remainingArgs = args.take(holeArgIdx) ++ args.drop(holeArgIdx + 1)
            remainingArgs.zip(argTemplates).foldLeft(Option(hSubst)) {
              case (None, _) => None
              case (Some(accSubst), (remainingArg, template)) =>
                val instance = template.replaceIndex(cursor, realMax.toString).substitute(accSubst - innerMaxVar)
                instance.matches(remainingArg).map(accSubst ++ _)
            }
          }
          // otherwise, since max is unbounded, let's try to match the seed as long as the min was matched
          .orElse {
            for
              _ <- Option.when(minOccurs <= 0)(())
              s <- holeSeed
              subst <- s.matches(otherTerm)
            yield
              subst + (maxVar -> Constant((minIndex - 1).asInstanceOf[TConstant]))
          }
      case (_, /*holeIsMax*/true, _, Function(`function`, args*))
        if maxIndex.fold(_ => true, n => minIndex < n.value)
      =>
        val newMaxIndex = maxIndex.fold(v => v.copy(min = v.min - 1), n => n)
        val remainingArgs = args.take(holeArgIdx) ++ args.drop(holeArgIdx + 1)
        remainingArgs.zip(argTemplates)
          .foldLeft(Option(Map[String, TTerm]())) {
            case (None, _) => None
            case (Some(accSubst), (remainingArg, template)) =>
              val instance = template.replaceIndex(cursor, minIndex.toString).substitute(accSubst)
              instance.matches(remainingArg).map(accSubst ++ _)
          }
          .flatMap { rSubst =>
            Range(function, cursor, holeArgIdx, argTemplates, minIndex + 1, newMaxIndex, true, holeSeed)
              .substitute(rSubst)
              .matches(args(holeArgIdx))
              .map(rSubst ++ _)
          }
      case (_, _, holeSeed, Range(`function`, otherCursor, `holeArgIdx`, otherArgTemplates, `minIndex`, otherMaxIndex, `holeIsMax`, otherHoleSeed))
        if argTemplates.size == otherArgTemplates.size
      =>
        for
          tSubst <- argTemplates.zip(otherArgTemplates).foldLeft(Option(Map[String, TTerm]())) {
            case (None, _) => None
            case (Some(accSubst), (thisTemplate, otherTemplate)) =>
              thisTemplate.substitute(accSubst).matches(otherTemplate.replaceIndex(otherCursor, cursor))
                .map(accSubst ++ _.map(_ -> _.replaceIndex(otherCursor, cursor)))
          } 
          // only valid if none of the ranged-over variables got a substitution, as
          // those act more like constants from the range point of view 
          if tSubst.keys.collect(extractIndex.unlift).forall(_._2 != cursor)
          mSubst <- maxIndex match {
            case Index.Variable(maxVar, min) => otherMaxIndex match {
              case Index.Variable(`maxVar`, `min`) => Some(Map())
              case Index.Variable(otherMaxVar, `min`) => Some(Map(maxVar -> Variable(otherMaxVar)))
              case Index.Variable(_, _) => None
              case Index.Number(v) => Some(Map(maxVar -> Constant(v.asInstanceOf[TConstant])))
            }
            case _ => Option.when(maxIndex == otherMaxIndex)(Map[String, TTerm]())
          }
          sSubst <- (holeSeed, otherHoleSeed) match {
            case (None, None) => Some(Map())
            case (Some(s), Some(os)) => s.substitute(tSubst ++ mSubst).matches(os)
            case _ => None
          }
        yield
          tSubst ++ (mSubst ++ sSubst)
      case (_, _, _, Range(_, _, _, _ ,_, _, _, _)) => None
      case (Index.Variable(_, n), _, _, _) if n > 1 => None
      case (Index.Variable(maxVar, n), _, None, _) if n <= 1 && argTemplates.size == 1 =>
        argTemplates(0).replaceIndex(cursor, minIndex.toString).matches(otherTerm).map { subst =>
          subst + (maxVar -> Constant(minIndex.asInstanceOf[TConstant]))
        }
      case (Index.Variable(_, _), _, None, _) => None
      case (Index.Variable(maxVar, n), _, Some(holeSeed), _) if n <= 0 =>
        holeSeed.matches(otherTerm).map { subst =>
          subst + (maxVar -> Constant((minIndex - 1).asInstanceOf[TConstant]))
        }
      case (Index.Variable(_, _), _, Some(_), _) => None
      case (Index.Number(maxIndexVal), /*holeIsMax*/false, _, Function(`function`, args*)) if minIndex < maxIndexVal =>
        Range(function, cursor, holeArgIdx, argTemplates, minIndex, Index.Number(maxIndexVal - 1), false, holeSeed)
          .matches(args(holeArgIdx))
          .flatMap { holeSubst =>
            val remainingArgs = args.take(holeArgIdx) ++ args.drop(holeArgIdx + 1)
            remainingArgs.zip(argTemplates).foldLeft(Option(holeSubst)) {
              case (None, _) => None
              case (Some(accSubst), (remainingArg, template)) =>
                val instance = template.replaceIndex(cursor, maxIndexVal.toString).substitute(accSubst)
                instance.matches(remainingArg).map(accSubst ++ _)
            }
          }
      case (Index.Number(maxIndexVal), _, _, _) if minIndex == maxIndexVal =>
        val argInstances = argTemplates.map(_.replaceIndex(cursor, minIndex.toString))
        holeSeed match {
          case Some(s) =>
            val args = argInstances.patch(from = holeArgIdx, Seq(s), replaced = 0)
            Function(function, args*).matches(otherTerm)
          case None if argInstances.size == 1 => argInstances(0).matches(otherTerm)
          case None => None
        }
      case (Index.Number(_), _, _, _) => None
    }
    case (_, Range(function, cursor, holeArgIdx, argTemplates, minIndex, Index.Number(maxIndex), holeIsMax, holeSeed)) if minIndex == maxIndex =>
      val argInstances = argTemplates.map(_.replaceIndex(cursor, minIndex.toString))
      holeSeed match {
        case None if argInstances.size == 1 => this.matches(argInstances(0))
        case None => None
        case Some(s) =>
          val args = argInstances.patch(from = holeArgIdx, Seq(s), replaced = 0)
          this.matches(Function(function, args*))
      }
    case (Variable(_), _) if this == otherTerm => Some((Map()))
    case (Variable(name), _) => Some(Map(name -> otherTerm))
    case _ => None
  }

  def variables: Set[String] = this match {
    case Variable(name) => Set(name)
    case Constant(_) => Set()
    case Function(_, args*) => Set.concat(args.map(t => t.variables)*)
    case Range(params @ (_, _, _, _, _, maxIndex, _, _)) =>
      val indexVars = maxIndex.asVariable.map(_.name).toSet
      indexVars ++ getRangeElems(Range.apply.tupled(params), _.variables)
  }

  def substitute(subst: Map[String, TTerm]): TTerm = this match {
    case Variable(name) =>
      if subst.contains(name) then
        subst(name)
      else
        this
    case Constant(_) => this
    case Function(name, args*) => Function(name, args.map(_.substitute(subst))*)
    case Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed)
      if maxIndex.fold(v => subst.get(v.name).exists(_.isGround), n => true)
    =>
      val concreteMaxIndex = maxIndex match {
        case Index.Number(value) => value
        case Index.Variable(maxIndexVar, _) => subst(maxIndexVar) match {
          // TODO: clean this up by defining a proper Index data type
          case Constant(i: Int) => i
        }
      }
      val concreteArgs = (minIndex to concreteMaxIndex).map(i => argTemplates.map(_.replaceIndex(cursor, i.toString).substitute(subst)))
      val foldFn = (holeElem: TTerm, rArgs: Seq[TTerm]) => {
        val args = rArgs.patch(from = holeArgIdx, Seq(holeElem), replaced = 0)
        Function(function, args*)
      }
      (holeSeed, holeIsMax) match {
        case (Some(s), false) => concreteArgs.foldLeft(s.substitute(subst))(foldFn)
        case (Some(s), true) => concreteArgs.foldRight(s.substitute(subst))((r, h) => foldFn(h, r))
        case (None, false) => concreteArgs.tail.foldLeft(concreteArgs.head.head)(foldFn)
        case (None, true) => concreteArgs.init.foldRight(concreteArgs.last.head)((r, h) => foldFn(h, r))
      }
    case Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed) =>
      // otherwise, maxIndex is a variable and stays unbound
      // TODO: case where maxIndex is unbound but we can have a partial substitution
      // e.g. R:f[i in 1..k](V:e_i).substitute(e_3 -> C:a) ==> R:f[i in 4..k](f(f(V:e_1, V:e_2), C:a), V:e_i)
      val newTemplates = argTemplates.map(_.substitute(subst - cursor))
      val newMaxIndex = maxIndex match {
        case Index.Variable(name, min) if subst.contains(name) =>
          val newName = subst(name).asInstanceOf[TermVariable].name
          Index.Variable(newName, min)
        case _ => maxIndex 
      }
      val newSeed = holeSeed.map(_.substitute(subst))
      Range(function, cursor, holeArgIdx, newTemplates, minIndex, newMaxIndex, holeIsMax, newSeed)
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
      Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed),
      Range(otherFunction, otherCursor, otherHoleArgIdx, otherArgTemplates, otherMinIndex, otherMaxIndex, otherHoleIsMax, otherHoleSeed)
    ) =>
      if
        function == otherFunction &&
        holeArgIdx == otherHoleArgIdx &&
        argTemplates.size == otherArgTemplates.size &&
        minIndex == otherMinIndex &&
        holeIsMax == otherHoleIsMax
      then
        for
          maxIdxSubst <- (maxIndex, otherMaxIndex) match {
            case (Index.Variable(_, min), Index.Variable(_, otherMin)) if min != otherMin => None
            case (Index.Variable(name, _), Index.Variable(otherName, _)) if name == otherName => Some(Map[String, TTerm]())
            case (Index.Variable(name, _), Index.Variable(otherName, _)) => Some(Map(name -> Variable(otherName)))
            case (Index.Variable(name, min), Index.Number(i)) if i < min => None 
            case (Index.Variable(name, _), Index.Number(i)) => Some(Map(name -> Constant(i.asInstanceOf[TConstant])))
            case (Index.Number(i), Index.Number(otherI)) if i == otherI => Some(Map())
            case _ => None
          }
          seedSubst <- (holeSeed, otherHoleSeed) match {
            case (None, None) => Some(Map())
            case (Some(s), Some(otherS)) => s.substitute(maxIdxSubst).unifies(otherS.substitute(maxIdxSubst))
            case _ => None
          }
          templateSubst <- argTemplates.zip(otherArgTemplates).foldLeft(Option(seedSubst)) {
            case (None, _) => None
            case (Some(hSubst), (template, otherTemplate)) =>
              template.substitute(hSubst).unifies(otherTemplate.replaceIndex(otherCursor, cursor).substitute(hSubst))
          }
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
    val freshNamesSubst = Map.from(commonVarNames.map(name => name -> Variable("$" + name)))
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
    case Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed) =>
      val newTemplates =
        if cursor != oldIdxStr
        then argTemplates.map(_.replaceIndex(oldIdxStr, newIdxStr))
        else argTemplates
      val newSeed = holeSeed.map(_.replaceIndex(oldIdxStr, newIdxStr))
      val newMaxIndex = maxIndex match {
        case iv: Index.Variable if iv.name == oldIdxStr => iv.copy(name = newIdxStr)
        case _ => maxIndex
      }
      Range(function, cursor, holeArgIdx, newTemplates, minIndex, newMaxIndex, holeIsMax, newSeed)
  }

  override def toString(): String = this match {
    case Constant(value) => s"C:$value"
    case Variable(name) =>  s"V:$name"
    case Function(name, args*) => args.mkString(s"$name(", ", ", ")")
    case Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed) =>
      val seedStr = holeSeed.map(_.toString).getOrElse("_")
      val argStrs = argTemplates.take(holeArgIdx).map(_.toString) ++ 
        Seq(seedStr) ++ 
        argTemplates.drop(holeArgIdx).map(_.toString)
      val holeIsMaxStr = if holeIsMax then "<-" else "->"
      s"R:${function}[$cursor in $minIndex..$maxIndex]$holeIsMaxStr(${argStrs.mkString(", ")})"
  }
