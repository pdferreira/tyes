package tyes.runtime

trait CompositeType[+InnerT <: Type](val innerTypes: InnerT*) extends Type
