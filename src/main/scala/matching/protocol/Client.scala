package matching.protocol

import akka.actor.{Actor, Props}

import scala.collection.mutable

/**
  * Companion-object для [[Client]]
  * Содержит типы сообщений, обрабатываемых актором
  */
object Client {

  /**
    * Фабричный метод, создающий экземпляр актора и возвращающий ссылку на него
    *
    * @param name           имя клиента
    * @param initialBalance изначальный баланс по долларам
    * @param initialStocks  изначальный баланс по ценным бумагам
    * @return ссылка на актор
    */
  def props(name: String, initialBalance: Long, initialStocks: Map[String, Long]): Props =
    Props(new Client(name, initialBalance, initialStocks))

  /**
    * Сообщение для увеличения баланса клиента
    *
    * @param amount сумма увеличения баланса
    */
  final case class IncreaseBalance(amount: Long)

  /**
    * Сообщение для увеличения баланса клиента по ценной бумаге
    *
    * @param stock ценная бумага
    * @param count число ценных бумаг, на которое увеличивается баланс
    */
  final case class AddStocks(stock: String, count: Long)

  /**
    * Сообщение для уменьшения баланса клиента
    *
    * @param amount сумма уменьшения баланса
    */
  final case class DecreaseBalance(amount: Long)

  /**
    * Сообщение для уменьшения баланса клиента по ценной бумаге
    *
    * @param stock ценная бумага
    * @param count число ценных бумаг, на которое уменьшается баланс
    */
  final case class SubtractStocks(stock: String, count: Long)

  /**
    * Сообщение, запрашивающее состояние клиента (его балансы)
    */
  case object GetClientState

}

/**
  * Актор, описывающий клиента биржи
  *
  * @param name           имя клиента
  * @param initialBalance изначальный баланс по долларам
  * @param initialStocks  изначальный баланс по ценным бумагам
  */
class Client(name: String, initialBalance: Long, initialStocks: Map[String, Long]) extends Actor {

  import Client._

  /** Баланс клиента по долларам */
  var balance: Long = 0
  balance += initialBalance

  /** Балансы клиента по ценным бумагам */
  var stocks: mutable.Map[String, Long] = mutable.Map.empty[String, Long]
  stocks ++= initialStocks

  override def receive: Receive = {
    case IncreaseBalance(amount) =>
      balance += amount
    case AddStocks(stock, count) =>
      val oldCount = stocks.getOrElse(stock, 0L)
      stocks.put(stock, oldCount + count)
    case DecreaseBalance(amount) =>
      balance -= amount
    case SubtractStocks(stock, count) =>
      val oldCount = stocks(stock)
      stocks.put(stock, oldCount - count)
    case GetClientState =>
      sender() ! ClientState(name, balance, stocks.toMap)
  }
}
