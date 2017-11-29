sealed abstract class CardType

case object Visa extends CardType

case object MasterCard extends CardType

sealed abstract class PaymentMethod

case object Cash extends PaymentMethod

case class Cheque(number: String) extends PaymentMethod

case class Card(cardType: CardType, number: String) extends PaymentMethod

sealed trait Expr

case class Number(x: Int) extends Expr

case class Plus(a: Expr, b: Expr) extends Expr


object Expr {
  def eval(e: Expr):Int = e match {
    case Number(e) => e
    case Plus(a, b) => eval(a) + eval(b)
  }
}


class A(private val x: Int) {
  def equals(obj: Any) = obj match {
    case that: A => x == that.x
    case _ => false
  }
}



