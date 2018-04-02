package matching

import java.nio.file.{Files, Paths}

import akka.actor.{Actor, ActorLogging, Props}

import scala.collection.JavaConverters.asJavaCollection

/**
  * companion-object для [[ClientsPrinter]]
  */
object ClientsPrinter {

  /**
    * Фабричный метод, создающий экземпляр актора и возвращающий ссылку на него
    *
    * @param filePath путь для файла с балансами
    * @return ссылка на актор
    */
  def props(filePath: String): Props = Props(new ClientsPrinter(filePath))

  /**
    * Сообщение на вывод балансов клиентов
    *
    * @param clients список клиентов
    */
  final case class PrintMessage(clients: List[Client])

}

/**
  * Актор для вывода балансов клиентов в файл (и в консоль)
  *
  * @param filePath путь для файла с балансами
  */
class ClientsPrinter(filePath: String) extends Actor with ActorLogging {

  import ClientsPrinter._

  def receive: PartialFunction[Any, Unit] = {
    case msg@PrintMessage(clients) =>
      val lines = clients.map(Client.stringify)
      // FIXME: не очень хорошо вызывать блокирующую операцию внутри актора, но в рамках данной задачи можно
      Files.write(Paths.get(filePath), asJavaCollection(lines))
      clients.foreach(client => log.info(Client.stringify(client)))
      // FIXME: не очень хорошо вызывать остановку системы акторов, но в рамках данной задачи можно
      context.system.terminate()
  }
}
