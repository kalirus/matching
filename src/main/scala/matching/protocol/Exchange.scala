package matching.protocol

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import matching.protocol.Client._
import matching.protocol.Exchange.{AddClient, AddOrder, GetAllClientsStates}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

/**
  * companion-object для актора [[Exchange]]
  * Содержит типы сообщений, обрабатываемых актором
  */
object Exchange {

  /**
    * Фабричный метод, создающий экземпляр актора и возвращающий ссылку на него
    *
    * @return ссылка на актор
    */
  def props(): Props = Props(new Exchange())

  /**
    * Сообщение добавления нового клиента на биржу
    *
    * @param client клиент
    */
  final case class AddClient(client: ClientState)

  /**
    * Сообщение добавления новой заяки на биржу
    *
    * @param order заявка
    */
  final case class AddOrder(order: Order)

  /**
    * Сообщение для получения балансов всех клиентов
    */
  case object GetAllClientsStates

}

/**
  * Актор, моделирующий работу биржи.
  * Инкапсулирует балансы клиентов и очереди по заявкам
  */
class Exchange extends Actor {

  /**
    * Хэш-карта, хранящая клиентов биржи.
    * Используем хэш, хранящий порядок добавления элементов, чтобы вывести клиентов в том же порядке, в котором они были добавлены на биржу.
    * Ключ - наименование клиента, значение - экземпляр клиента (актор).
    * Так как состояние хэша изменяется только в методе [[receive]], можем позволить использовать mutable коллекцию, чтобы
    * не было большой нагрузки на garbage collector при изменениях.
    */
  val clients: mutable.Map[String, ActorRef] = new mutable.LinkedHashMap[String, ActorRef]()

  /**
    * Хэш-карта заявок на покупку, ожидающих своей очереди.
    * Ключ - наименование ценной бумаги, значение - очередь из заявок на покупку по ней.
    * Так как состояние хэша изменяется только в методе [[receive]], можем позволить использовать mutable коллекцию, чтобы
    * не было большой нагрузки на garbage collector при изменениях.
    */
  val unprocessedBuyOrders: mutable.Map[String, mutable.Queue[Order]] = mutable.Map.empty[String, mutable.Queue[Order]]

  /**
    * Хэш-карта заявок на продажу, ожидающих своей очереди.
    * Ключ - наименование ценной бумаги, значение - очередь из заявок на продажу по ней.
    * Так как состояние хэша изменяется только в методе [[receive]], можем позволить использовать mutable коллекцию, чтобы
    * не было большой нагрузки на garbage collector при изменениях.
    */
  val unprocessedSellOrders: mutable.Map[String, mutable.Queue[Order]] = mutable.Map.empty[String, mutable.Queue[Order]]

  /**
    * Возвращает очередь с заявками на покупку для ценной бумаги
    *
    * @param stock наименование ценной бумаги
    * @return очередь с заявками на покупку
    */
  private[this] def getStockBuyQueue(stock: String) = {
    unprocessedBuyOrders.getOrElseUpdate(stock, new mutable.Queue[Order]())
  }

  /**
    * Возвращает очередь с заявками на продажу для ценной бумаги
    *
    * @param stock наименование ценной бумаги
    * @return очередь с заявками на продажу
    */
  private[this] def getStockSellQueue(stock: String) = {
    unprocessedSellOrders.getOrElseUpdate(stock, new mutable.Queue[Order]())
  }

  override def receive: PartialFunction[Any, Unit] = {
    case AddClient(client) =>
      val clientActorRef = context.actorOf(Client.props(client.name, client.balance, client.stocks))
      clients.put(client.name, clientActorRef)
    case AddOrder(order) =>
      processOrder(order)
    case GetAllClientsStates =>
      implicit val timeout: Timeout = Timeout(10 seconds)
      implicit val ec: ExecutionContext = context.dispatcher
      val f: List[Future[ClientState]] = for {
        client <- clients.values.toList
      } yield (client ? GetClientState).mapTo[ClientState]
      pipe(Future.sequence(f)) to sender()
  }

  /**
    * Вызывается при добавлении новой заявки на биржу.
    * Если нет заявки, соответствующей пришедшей, добавляем заявку в очередь ([[unprocessedSellOrders]] или [[unprocessedBuyOrders]])
    *
    * @param order заявка
    */
  private[this] def processOrder(order: Order) {
    order.operation match {
      case Sell =>
        val queue: mutable.Queue[Order] = getStockBuyQueue(order.stock)
        val matched = queue.dequeueFirst(matchOrders(_, order))
        if (matched.nonEmpty) {
          executeTransaction(matched.get, order)
        } else {
          getStockSellQueue(order.stock).enqueue(order)
        }
      case Buy =>
        val queue: mutable.Queue[Order] = getStockSellQueue(order.stock)
        val matched = queue.dequeueFirst(matchOrders(_, order))
        if (matched.nonEmpty) {
          executeTransaction(order, matched.get)
        } else {
          getStockBuyQueue(order.stock).enqueue(order)
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
  private[this] def matchOrders(order1: Order, order2: Order): Boolean = {
    order1.stock == order2.stock && order1.price == order2.price && order1.count == order2.count && order1.client != order2.client
  }

  /**
    * Обрабатывает две сопоставившиеся заявки и обновляет балансы клиентов
    *
    * @param buyOrder  заявка на покупку
    * @param sellOrder заявка на продажу
    */
  private[this] def executeTransaction(buyOrder: Order, sellOrder: Order) {
    val buyClient = clients(buyOrder.client)
    val sellClient = clients(sellOrder.client)

    val count = buyOrder.count
    val stock = buyOrder.stock
    val amount = buyOrder.count * buyOrder.price

    buyClient ! DecreaseBalance(amount)
    buyClient ! AddStocks(stock, count)

    sellClient ! SubtractStocks(stock, count)
    sellClient ! IncreaseBalance(amount)
  }
}
