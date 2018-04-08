package matching

import java.nio.file.{Files, Paths}

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import matching.protocol.{ClientState, Exchange, Order}

import scala.collection.JavaConverters.asJavaCollection
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps

/**
  * Main класс для запуска матчинга заявок
  */
object Matching extends App {

  import matching.protocol.Exchange._

  val system: ActorSystem = ActorSystem("matching")
  val settings = system.settings

  val clientsFilePath = settings.config.getString("matching.clients_file")
  val ordersFilePath = settings.config.getString("matching.orders_file")
  val resultsFilePath = settings.config.getString("matching.results_file")

  val exchange: ActorRef = system.actorOf(Exchange.props(), "exchangeActor")

  Source.fromFile(clientsFilePath, "UTF-8").getLines().foreach(line => {
    val client = ClientState.fromString(line)
    exchange ! AddClient(client)
  })

  Source.fromFile(ordersFilePath, "UTF-8").getLines().foreach(line => {
    val order = Order.fromString(line)
    exchange ! AddOrder(order)
  })

  implicit val timeout: Timeout = Timeout(10 seconds)
  implicit val ec: ExecutionContext = system.dispatcher
  ask(exchange, GetAllClientsStates).mapTo[List[ClientState]]
    .foreach(clients => {
      val lines = clients.map(ClientState.stringify)
      Files.write(Paths.get(resultsFilePath), asJavaCollection(lines))
      clients.foreach(client => println(ClientState.stringify(client)))
      system.terminate()
    })
}
