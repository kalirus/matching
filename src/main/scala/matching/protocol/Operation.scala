package matching.protocol

/**
  * Операция по ценным бумагам
  */
sealed trait Operation

/**
  * Операция покупки ценных бумаг
  */
case object Buy extends Operation

/**
  * Операция продажи ценных бумаг
  */
case object Sell extends Operation

object Operation {

  /**
    * Возвращает объект операции из символьного представления
    *
    * @param symbol символ операции
    * @return объект операции
    */
  def apply(symbol: String): Operation = {
    symbol match {
      case "s" => Sell
      case "b" => Buy
    }
  }
}
