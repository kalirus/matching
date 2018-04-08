package matching

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import matching.protocol.Exchange.{AddClient, AddOrder, GetAllClientsStates}
import matching.protocol._
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}

import scala.concurrent.duration._
import scala.language.postfixOps

class MatchingSpec(_system: ActorSystem) extends TestKit(_system)
  with Matchers
  with FlatSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("MatchingSpec"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  /**
    * Тестирует [[Exchange]] актор
    */
  "An Exchange Clients" should "have valid balances" in {

    val clients = Seq(
      ClientState("C1", 100, Map("A" -> 10, "B" -> 20, "C" -> 30, "D" -> 40)),
      ClientState("C2", 200, Map("A" -> 40, "B" -> 30, "C" -> 20, "D" -> 10)),
      ClientState("C3", 300, Map("A" -> 50, "B" -> 60, "C" -> 70, "D" -> 80))
    )

    val orders = Seq(
      Order(1, "C1", Buy, "A", 1, 50),
      Order(2, "C2", Buy, "B", 2, 20),
      Order(3, "C3", Sell, "D", 2, 20),
      Order(4, "C3", Sell, "A", 1, 50),
      Order(5, "C2", Buy, "D", 2, 20),
      Order(6, "C2", Buy, "C", 1, 100),
      Order(7, "C3", Sell, "D", 2, 20),
      Order(8, "C1", Buy, "C", 2, 20),
      Order(9, "C2", Sell, "C", 1, 100),
      Order(10, "C1", Sell, "B", 2, 20)
    )

    val results = List(
      ClientState("C1", 90, Map("A" -> 60, "B" -> 0, "C" -> 30, "D" -> 40)),
      ClientState("C2", 120, Map("A" -> 40, "B" -> 50, "C" -> 20, "D" -> 30)),
      ClientState("C3", 390, Map("A" -> 0, "B" -> 60, "C" -> 70, "D" -> 60))
    )

    val testProbe = TestProbe()
    val exchangeActor = system.actorOf(Exchange.props())

    clients.map(AddClient).foreach(message => testProbe.send(exchangeActor, message))
    orders.map(AddOrder).foreach(message => testProbe.send(exchangeActor, message))

    testProbe.send(exchangeActor, GetAllClientsStates)
    testProbe.expectMsg(10 seconds, results)
  }
}
