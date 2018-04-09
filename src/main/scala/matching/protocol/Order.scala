package matching.protocol

import java.util.concurrent.atomic.AtomicLong

/**
  * Заявка на продажу / покупку ценных бумаг на бирже
  *
  * @param id        идентификатор заявки
  * @param client    имя клиента, выставившего заявку
  * @param operation операция
  * @param stock     наименование ценной бумаги
  * @param price     цена заявки (за одну штуку)
  * @param count     количество продаваемых или покупаемых ценных бумаг
  */
case class Order(id: Long, client: String, operation: Operation, stock: String, price: Long, count: Long)

object Order {

  /** Разделитель полей при чтении заявок из текстового файла */
  final val delimiter: Char = '\t'

  /** Генератор идентификаторов для заявок */
  private final val idGenerator: AtomicLong = new AtomicLong()

  /**
    * Создает экземпляр заявки из ее текстового представления в виде строки
    *
    * @param s строка
    * @return заявка
    */
  def fromString(s: String): Order = {
    val array: Array[String] = s.split(delimiter)
    val client: String = array(0)
    val operation: Operation = Operation(array(1))
    val stock: String = array(2)
    val price: Long = array(3).toLong
    val count: Long = array(4).toLong
    Order(idGenerator.incrementAndGet(), client, operation, stock, price, count)
  }
}
