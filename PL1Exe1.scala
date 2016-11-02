object PL1 {


  object Date {
    /*
    It takes two dates and evaluates to true or false. It evaluates to true if
    the rst argument is a date that comes before the second argument.
     */
    def isOlder(d1: Date, d2: Date): Boolean = {
      if (d1.year < d2.year) true
      else if (d1.year == d2.year && d1.month < d2.month) true
      else if (d1.year == d2.year && d1.month == d2.month && d1.day < d2.day) true
      else false
    }

    /*
    It takes a list of dates and a month (i.e., an int) and returns
    how many dates in the list are in the given month
     */
    def numInMonth(dates: List[Date], month: Int): Int = {
      dates.foldLeft(0)((acc, next) => {
        if (month == next.month) acc + 1
        else acc
      })
    }

    /*
    It takes a list of dates and a list of months (i.e., an int list)
    and returns the number of dates in the list of dates that are in any of the months in the list of months.
     */
    def numInMonths(dates: List[Date], month: List[Int]): Int = {
      month.foldLeft(0)((acc, next) => {
        numInMonth(dates, next) + acc
      })
    }

    /*
    It takes a list of dates and a month (i.e., an int) and returns a
    list holding the dates from the argument list of dates that are in the month. The returned list should
    contain dates in the order they were originally given.
     */
    def datesInMonth(dates: List[Date], month: Int): List[Date] = {
      dates.filter(_.month == month)
    }

    /*
    It that takes a list of dates and a list of months (i.e., an int list)
    and returns a list holding the dates from the argument list of dates that are in any of the months in
    the list of months.
     */
    def datesInMonths(dates: List[Date], months: List[Int]): List[Date] = {
      for (d <- dates;
           m <- months if (d.month == m)
      ) yield d
    }

    /*
    It takes a list of strings and an int n and returns the nth element of the
    list where the head of the list is 1st
     */
    def getNth(strings: List[String], n: Int): String = strings(n - 1)

    val _months = List("January", "February", "March", "April",
      "May", "June", "July", "August", "September", "October", "November", "December")

    /*

     */
    def dateToString(date: Date): String = {
      val y = date.year
      val m = getNth(_months, date.month)
      val d = date.day
      s"$m $d,$y"
    }

    /*
    It takes an int called sum, which you can assume
    is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
    You should return an int n such that the rst n elements of the list add to less than sum, but the rst
    n+1 elements of the list add to sum or more.
     */
    def numBeforeSum(sum: Int, list: List[Int]): Int = {
      def helper(acc: Int, nth: Int, l: List[Int]): Int = l match {
        case Nil => throw new RuntimeException("Unexpected sum value.")
        //just over sum
        case x :: xs if (acc > sum) => nth
        case x :: xs => helper(acc + x, nth + 1, xs)
      }
      helper(0, 0, list) - 1
    }

    /*
    It takes a day of year (i.e., an int between 1 and 365) and returns
    what month that day is in (1 for January, 2 for February, etc.)
     */
    def whatMonth(dayOfYear: Int): Int = {
      numBeforeSum(dayOfYear, List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)) + 1
    }

    /*
    It takes two days of the year day1 and day2 and returns an int list
    [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
    of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.
     */
    def monthRange(day1: Int, day2: Int): List[Int] = {
      if (day1 > day2) List()
      else whatMonth(day1) :: monthRange(day1 + 1, day2)
    }

    /*
    It takes a list of dates and evaluates to an (int*int*int) option. It
    evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.
     */
    def oldest(dates: List[Date]): Option[Date] = dates match {
      case Nil => None
      case _ => Some(dates.sortWith((a, b) => isOlder(a, b))(0))
    }


  }

  case class Date(year: Int, month: Int, day: Int)

}
