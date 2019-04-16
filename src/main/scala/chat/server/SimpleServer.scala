package chat.server

import java.io.File

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object SimpleServer {
  def props(): Props = Props(new SimpleServer)
}

class SimpleServer extends Actor with ActorLogging {
  import chat.client.SimpleClient._

  override def preStart(): Unit = {
    log.info("Started server actor")
    log.info(akka.serialization.Serialization.serializedActorPath(self))
  }

  override def postStop(): Unit = log.info("Stopped server actor")

  override def receive: Receive = {
    case TextLineMsg(line) =>
      println(s"Received: $line")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val portConfig = ConfigFactory.load("server")
    val system = ActorSystem("chat-server-system", portConfig)

    val serverActorRef = system.actorOf(SimpleServer.props(), "server")
  }
}