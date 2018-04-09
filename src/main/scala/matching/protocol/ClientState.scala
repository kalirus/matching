package matching.protocol

/**
  * Описывает состояние клиента биржи
  *
  * @param name    имя клиента
  * @param balance баланс клиента по долларам
  * @param stocks  баланс клиента по ценным бумагам
  */
case class ClientState(name: String, balance: Long, stocks: Map[String, Long])

object ClientState {

  /** Разделитель полей при чтении объектов из текстового файла */
  final val delimiter: Char = '\t'

  /**
    * Создает объект состояния клиента биржа из его текстового представления в виде строки
    *
    * @param s строка
    * @return состояние клиента биржи
    */
  def fromString(s: String): ClientState = {
    val array: Array[String] = s.split(delimiter)
    val name: String = array(0)
    val balance: Long = array(1).toLong
    val stocks: Map[String, Long] = Map(
      "A" -> array(2).toLong,
      "B" -> array(3).toLong,
      "C" -> array(4).toLong,
      "D" -> array(5).toLong
    )
    new ClientState(name, balance, stocks)
  }

  /**
    * Представляет состояние клиента в виде строки данных, совместимой с методом [[fromString()]]
    *
    * @param clientState состояние клиента биржи
    * @return строка данных
    */
  def stringify(clientState: ClientState): String = {
    val values = Array(clientState.name, clientState.balance, clientState.stocks("A"), clientState.stocks("B"), clientState.stocks("C"), clientState.stocks("D"))
    values.mkString(delimiter.toString)
  }
}
