package utils

object CollectionExtensions:
  
  extension [A, B](col: Seq[(A, B)])

    def distribute: (Seq[A], Seq[B]) = col.foldLeft((Seq[A](), Seq[B]())) {
      case ((as, bs), (a, b)) => (as :+ a, bs :+ b)
    }
