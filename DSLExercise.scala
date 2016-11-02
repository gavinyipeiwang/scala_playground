trait Currency {
  def code: String
}

object Currency {

  object CNY extends Currency {
    def code: String = "CNY"
  }

  object AUD extends Currency {
    def code: String = "AUD"
  }

  object USD extends Currency {
    def code: String = "USD"
  }

  def apply(s: String): Currency = s.toUpperCase match {
    case "CNY" => CNY
    case "AUD" => AUD
    case "USD" => USD
  }

}

type Conversion = Map[(Currency, Currency), BigDecimal]

import Currency._

val conversion: Conversion = Map(
  (CNY, AUD) -> 0.1936,
  (CNY, USD) -> 0.1475,
  (AUD, USD) -> 0.7623
)


case class Converter(conversion: Conversion) {
  def convert(from: Currency, to: Currency): BigDecimal = {
    if (from == to) 1
    else conversion.getOrElse((from, to), 1 / convert(to, from))
  }
}

case class Money(amount: BigDecimal, currency: Currency)(implicit converter: Converter) {

  def +(thatMoney: Money): Money = performOperation(thatMoney, (a, b) => a + b)

  def performOperation(thatMoney: Money, operation: (BigDecimal, BigDecimal) => BigDecimal): Money = {
    thatMoney match {
      case Money(amt, cur) if cur == currency => Money(operation(amount, amt), currency)
      case Money(amt, cur) => performOperation(thatMoney.to(currency), operation)
    }
  }

  def to(thatCurrency: Currency): Money = {
    val rate = converter.convert(currency, thatCurrency)
    Money(amount * rate, thatCurrency)
  }
}

implicit class BigDecimalOps(value: BigDecimal) {
  def apply(currency: Currency)(implicit converter: Converter): Money = Money(value, currency)
}

implicit class IntOps(value: Int) {
  def apply(currency: Currency)(implicit converter: Converter): Money = (value: BigDecimal).apply(currency)
}

implicit class DoubleOps(value: Double) {
  def apply(currency: Currency)(implicit converter: Converter): Money = (value: BigDecimal).apply(currency)
}


implicit val converter = Converter(conversion)


val result = 100 (CNY) + 100 (AUD)










