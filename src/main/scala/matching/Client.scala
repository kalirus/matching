package matching

/**
  * Клиент биржи
  *
  * @param name    имя клиента
  * @param balance баланс клиента по долларам
  * @param stocks  баланс клиента по ценным бумагам
  */
case class Client(name: String, balance: Long, stocks: Map[String, Long]) {}

object Client {

  /** Разделитель полей при чтении клиентов из текстового файла */
  private final val delimiter: Char = '\t'

  /**
    * Создает экземпляр клиента из его текстового представления в виде строки
    *
    * @param s строка
    * @return клиент
    */
  def fromString(s: String): Client = {
    val array: Array[String] = s.split(delimiter)
    val name: String = array(0)
    val balance: Long = array(1).toLong
    val stocks: Map[String, Long] = Map(
      "A" -> array(2).toLong,
      "B" -> array(3).toLong,
      "C" -> array(4).toLong,
      "D" -> array(5).toLong
    )
    new Client(name, balance, stocks)
  }

  /**
    * Представляет экземпляр клиента в виде строки данных, совместимой с методом [[fromString()]]
    *
    * @param client клиент
    * @return строка данных
    */
  def stringify(client: Client): String = {
    s"${client.name}$delimiter${client.balance}$delimiter${client.stocks("A")}$delimiter${client.stocks("B")}$delimiter${client.stocks("C")}$delimiter${client.stocks("D")}"
  }
}
