object PL2 {


  /*
  It takes a string and a string list. Return NONE if the
  string is not in the list, else return SOME lst where lst is identical to the argument list except the string
  is not in it.
   */
  def all_except_option(s: String, list: List[String]): Option[List[String]] = list match {
    case head :: tail => if (head == s) Some(tail)
    else all_except_option(s, tail) match {
      case None => None
      case Some(xs) => Some(head :: xs)
    }
    case Nil => None
  }

  /*
  It takes a string list list (a list of list of strings, the
  substitutions) and a string s and returns a string list. The result has all the strings that are in
  some list in substitutions that also has s, but s itself should not be in the result. Example:
  get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
  "Fred")
  (* answer: ["Fredrick","Freddie","F"] *)
   */
  def get_substitutions1(lists: List[List[String]], string: String): List[String] = {
    lists.flatMap((list) => {
      all_except_option(string, list) match {
        case None => None
        case Some(x) => x
      }
    })
  }

  /*
  get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
  local helper function.
   */
  def get_substitutions2(lists: List[List[String]], string: String): List[String] = {
    def helper(l: List[List[String]], acc: List[String]): List[String] = l match {
      case head :: tail => all_except_option(string, head) match {
        case None => helper(tail, acc)
        case Some(x) => helper(tail, x ::: acc)
      }
      case Nil => acc
    }
    helper(lists, List())
  }

  /*
  It takes a string list list of substitutions (as in parts (b) and
  (c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
  names (type {first:string,middle:string,last:string} list).
   */
  type Name = (String, String, String)

  def similar_names(lists: List[List[String]], name: Name): List[Name] = {
    get_substitutions2(lists, name._1).foldLeft(List(name))((acc, next) => {
      (next, name._2, name._3) :: acc
    })
  }


}
