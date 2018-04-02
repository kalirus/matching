package matching

import akka.actor.{Actor, ActorRef, Props}
import matching.ClientsPrinter.PrintMessage
import matching.Exchange.{AddClient, AddOrder, PrintClients}

import scala.collection.mutable

/**
  * companion-object для актора [[Exchange]]
  * Содержит типы сообщений, обрабатываемых актором
  */
object Exchange {

  /**
    * Фабричный метод, создающий экземпляр актора и возвращающий ссылку на него
    *
    * @param printer актор для вывода балансов клиентов
    * @return ссылка на актор
    */
  def props(printer: ActorRef): Props = Props(new Exchange(printer))

  /**
    * Сообщение добавления нового клиента на биржу
    *
    * @param client клиент
    */
  final case class AddClient(client: Client)

  /**
    * Сообщение добавления новой заяки на биржу
    *
    * @param order заявка
    */
  final case class AddOrder(order: Order)

  /**
    * Сообщение для вывода балансов клиентов
    */
  case object PrintClients

}

/**
  * Актор, моделирующий работу биржи.
  * Инкапсулирует балансы клиентов и очереди по заявкам
  *
  * @param printer актор для вывода балансов клиентов при получении сообщения класса [[PrintClients]]
  */
class Exchange(printer: ActorRef) extends Actor {

  /**
    * Хэш-карта, хранящая клиентов биржи.
    * Используем хэш, хранящий порядок добавления элементов, чтобы вывести клиентов в том же порядке, в котором они были добавлены на биржу.
    * Ключ - наименование клиента, значение - экземпляр клиента.
    * Так как состояние хэша изменяется только в методе [[receive]], можем позволить использовать mutable коллекцию, чтобы
    * не было большой нагрузки на garbage collector при изменениях.
    */
  val clients: mutable.LinkedHashMap[String, Client] = new mutable.LinkedHashMap[String, Client]()

  /**
    * Хэш-карта заявок на покупку, ожидающих своей очереди.
    * Ключ - идентификатор заявки, значение - сама заявка.
    * Так как состояние хэша изменяется только в методе [[receive]], можем позволить использовать mutable коллекцию, чтобы
    * не было большой нагрузки на garbage collector при изменениях.
    */
  val unprocessedBuyOrders: mutable.LinkedHashMap[Long, Order] = new mutable.LinkedHashMap[Long, Order]()

  /**
    * Хэш-карта заявок на продажу, ожидающих своей очереди.
    * Ключ - идентификатор заявки, значение - сама заявка.
    * Так как состояние хэша изменяется только в методе [[receive]], можем позволить использовать mutable коллекцию, чтобы
    * не было большой нагрузки на garbage collector при изменениях.
    */
  val unprocessedSellOrders: mutable.LinkedHashMap[Long, Order] = new mutable.LinkedHashMap[Long, Order]()

  def receive: PartialFunction[Any, Unit] = {
    case AddClient(client) =>
      clients.put(client.name, client)
    case AddOrder(order) =>
      processOrder(order)
    case PrintClients =>
      printer ! PrintMessage(clients.values.toList)
  }

  /**
    * Вызывается при добавлении новой заявки на биржу.
    * Если нет заявки, соответствующей пришедшей, добавляем заявку в очередь ([[unprocessedSellOrders]] или [[unprocessedBuyOrders]])
    *
    * @param order заявка
    */
  private def processOrder(order: Order) {
    order.operation match {
      case Sell =>
        val matched = unprocessedBuyOrders.values.find(matchOrders(_, order))
        if (matched.nonEmpty) {
          processMatch(matched.get, order)
          unprocessedBuyOrders -= matched.get.id
        } else {
          unprocessedSellOrders += (order.id -> order)
        }
      case Buy =>
        val matched = unprocessedSellOrders.values.find(matchOrders(_, order))
        if (matched.nonEmpty) {
          processMatch(order, matched.get)
          unprocessedSellOrders -= matched.get.id
        } else {
          unprocessedBuyOrders += (order.id -> order)
        }
    }
  }

  /**
    * Проверяет две заявки на соответствие.
    * Заявки для типа ценной бумаги сопоставляются только по полному совпадению цены и количества.
    *
    * @param order1 первая заявка
    * @param order2 вторая заявка
    * @return true - если заявки соответсвуют, false - иначе
    */
  private def matchOrders(order1: Order, order2: Order): Boolean = {
    order1.stock == order2.stock && order1.price == order2.price && order1.count == order2.count && order1.client != order2.client
  }

  /**
    * Обрабатывает две сопоставившиеся заявки и обновляет балансы клиентов
    *
    * @param buyOrder  заявка на покупку
    * @param sellOrder заявка на продажу
    */
  private def processMatch(buyOrder: Order, sellOrder: Order) {
    val count = buyOrder.count
    val amount = buyOrder.count * buyOrder.price
    val stock = buyOrder.stock

    val buyClient = clients(buyOrder.client)
    val buyClientNewBalance = buyClient.balance - amount
    val buyClientNewStocks = buyClient.stocks + (stock -> (buyClient.stocks(stock) + count))
    val newBuyClient = buyClient.copy(balance = buyClientNewBalance, stocks = buyClientNewStocks)
    clients += (newBuyClient.name -> newBuyClient)

    val sellClient = clients(sellOrder.client)
    val sellClientNewBalance = sellClient.balance + amount
    val sellClientNewStocks = sellClient.stocks + (stock -> (sellClient.stocks(stock) - count))
    val newSellClient = sellClient.copy(balance = sellClientNewBalance, stocks = sellClientNewStocks)
    clients += (newSellClient.name -> newSellClient)
  }
}
