object Exercise21 {

  //get the greatest common divisor of two numbers
  def gcd(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => gcd(b, a % b)
  }


  class RationalNumber(n: Int, d: Int) {
    val numer = n
    val denom = d

    override def equals(other: Any): Boolean = other match {
      case that: RationalNumber =>
        (that.isInstanceOf[RationalNumber]) &&
          numer == that.numer &&
          denom == that.denom
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(numer, denom)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  object RationalNumber {
    def apply(numer: Int, denom: Int): RationalNumber = {
      val c = gcd(numer, denom)
      new RationalNumber(numer / c, denom / c)
    }
  }

  def add_rat(x: RationalNumber, y: RationalNumber): RationalNumber = {
    new RationalNumber(x.numer * y.denom + y.numer * x.denom, x.denom * y.denom)
  }

  def sub_rat(x: RationalNumber, y: RationalNumber): RationalNumber = {
    new RationalNumber(x.numer * y.denom - y.numer * x.denom, x.denom * y.denom)
  }

  def mul_rat(x: RationalNumber, y: RationalNumber): RationalNumber = {
    new RationalNumber(x.numer * y.numer, x.denom * y.denom)
  }

  def div_rat(x: RationalNumber, y: RationalNumber): RationalNumber = {
    new RationalNumber(x.numer * y.denom, x.denom * y.numer)
  }

  def equal_rat(x: RationalNumber, y: RationalNumber): Boolean = {
    x.numer * y.denom == x.denom * y.numer
  }

  def print_rat(x: RationalNumber): String = "%d/%d".format(x.numer, x.denom)

  case class Point(x: Double, y: Double)

  case class Segment(start: Point, end: Point)

  /*
  Exercise 2.2
  Define the representation of representing segment and points.
   */
  object Segment {
    def midpoint_segment(segment: Segment): Point = {
      Point((segment.start.x + segment.end.x) / 2, (segment.start.y + segment.end.y) / 2)
    }

    def print_point(point: Point): String = {
      val x = point.x
      val y = point.y
      s"($x,$y)"
    }
  }

  case class Interval(lowerBound: Double, upperBound: Double)

  object Interval {
    def add_interval(x: Interval, y: Interval): Interval = {
      Interval(x.lowerBound + y.lowerBound, x.upperBound + y.upperBound)
    }

    def sub_interval(x: Interval, y: Interval): Interval = {
      Interval(
        if (x.lowerBound < y.lowerBound) y.lowerBound else x.lowerBound,
        if (x.upperBound < y.upperBound) x.upperBound else y.upperBound
      )
    }

    def mul_interval(x: Interval, y: Interval): Interval = {
      val products = List(
        x.lowerBound * y.lowerBound,
        x.lowerBound * y.upperBound,
        x.upperBound * y.lowerBound,
        x.upperBound * y.upperBound
      ).sortWith(_ < _)
      Interval(products(0), products(3))
    }

    private def reciprocal(x: Interval) = {
      if (x.lowerBound == 0 || x.upperBound == 0) throw new RuntimeException("Lower bound or upper bound can't be 0.")
      Interval(1.0 / x.lowerBound, 1.0 / x.upperBound)
    }

    def div_interval(x: Interval, y: Interval) = mul_interval(x, reciprocal(y))

    def center(x: Interval): Double = (x.lowerBound + x.upperBound) / 2

    def width(x: Interval): Double = (x.upperBound - x.lowerBound) / 2

    def make_center_width(c: Double, width: Double): Interval = Interval(c - width, c + width)

    def make_center_percent(c: Double, percent: Double): Interval = Interval(c - c * percent, c + c * percent)

  }

}
