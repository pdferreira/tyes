package utils

object StringExtensions:

  extension (s: String)

    def decapitalize: String =
      if s.head.isLower 
      then s 
      else s.head.toLower + s.substring(1)
