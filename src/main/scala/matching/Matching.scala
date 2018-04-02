package matching

import akka.actor.{ActorRef, ActorSystem}

import scala.io.Source

/**
  * Main класс для запуска матчинга заявок
  */
object Matching extends App {

  import Exchange._

  val system: ActorSystem = ActorSystem("matching")
  val settings = system.settings

  val clientsFilePath = settings.config.getString("matching.clients_file")
  val ordersFilePath = settings.config.getString("matching.orders_file")
  val resultsFilePath = settings.config.getString("matching.results_file")

  val printer: ActorRef = system.actorOf(ClientsPrinter.props(resultsFilePath), "printerActor")
  val exchange: ActorRef = system.actorOf(Exchange.props(printer), "exchangeActor")

  Source.fromFile(clientsFilePath, "UTF-8").getLines().foreach(line => {
    val client = Client.fromString(line)
    exchange ! AddClient(client)
  })

  Source.fromFile(ordersFilePath, "UTF-8").getLines().foreach(line => {
    val order = Order.fromString(line)
    exchange ! AddOrder(order)
  })

  exchange ! PrintClients
}
