object PL3 {

  def only_capitals(xs: List[String]): List[String] = xs.filter(_.charAt(0).isUpper)

  def longest_string1(xs: List[String]): String = xs match {
    case Nil => ""
    case x :: xs => xs.foldLeft(x)((l, s) => if (l.length >= s.length) l else s)
  }

  def longest_string2(xs: List[String]): String = xs match {
    case Nil => ""
    case x :: xs => xs.foldLeft(x)((l, s) => if (l.length > s.length) l else s)
  }

  def longest_string_helper(f: (Int, Int) => Boolean)(xs: List[String]): String = xs match {
    case Nil => ""
    case x :: xs => xs.foldLeft(x)((l, s) => if (f(l.length, s.length)) l else s)
  }

  def longest_string3(xs: List[String]): String = longest_string_helper((x, y) => x >= y)(xs)

  def longest_string4(xs: List[String]): String = longest_string_helper((x, y) => x > y)(xs)

  def longest_capitalized(xs: List[String]): String = longest_string1(only_capitals(xs))

  def rev_string(s: String): String = s.toCharArray.reverse.mkString

  
}
