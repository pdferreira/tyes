package tyes.compiler.ir.rewrite

trait Rewrite[A]:

  def unapply(value: A): Option[A] = tryRewrite.applyOrElse(value, _ => None)

  protected val tryRewrite: PartialFunction[A, Option[A]]
