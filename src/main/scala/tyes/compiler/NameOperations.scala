package tyes.compiler

object NameOperations:

  def getDigitSuffix(name: String): Option[Int] = splitDigitSuffix(name)._2

  def splitDigitSuffix(name: String): (String, Option[Int]) = 
    val suffix = name.reverse.takeWhile(_.isDigit).reverse
    val prefix = name.dropRight(suffix.length)
    (prefix, if suffix.isEmpty then None else Some(suffix.toInt))

  def nameclash(name: String, existingNames: Set[String]): String =
    if existingNames.contains(name) then
      val (prefix, digitSuffix) = splitDigitSuffix(name)
      digitSuffix match {
        case None => nameclash(prefix + "2", existingNames)
        case Some(n) => nameclash(prefix + (n + 1), existingNames)
      }
    else
      name
