package utils

object StringExtensions:

  extension (s: String)

    def decapitalize: String =
      if s.head.isLower 
      then s 
      else s.head.toLower.toString + s.substring(1)
