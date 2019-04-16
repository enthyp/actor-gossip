package chat.client

import util.control.Breaks._
import akka.actor.{Actor, ActorLogging, ActorSelection, ActorSystem, PoisonPill, Props}
import com.typesafe.config.ConfigFactory


object SimpleClient {
  def props(clientId: Int, serverActorRef: ActorSelection): Props =
    Props(new SimpleClient(clientId, serverActorRef))

  final case class TextLineMsg(line: String)
}

class SimpleClient(clientId: Int, serverActorRef: ActorSelection) extends Actor with ActorLogging {
  import SimpleClient._

  override def preStart(): Unit = log.info("Started client actor")

  override def postStop(): Unit = log.info("Stopped client actor")

  override def receive: Receive = {
    case TextLineMsg(line) =>
      serverActorRef ! TextLineMsg(line)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val portConfig = ConfigFactory.load("client")
    val system = ActorSystem("chat-client-system", portConfig)

    val serverActorRef =
      system.actorSelection("akka.tcp://chat-server-system@127.0.0.1:2551/user/server")

    val clientActorRef = system.actorOf(SimpleClient.props(0, serverActorRef), "client")

    import SimpleClient._
    breakable {
      for (line <- scala.io.Source.stdin.getLines()) {
        line match {
          case "Okay, stop." =>
            clientActorRef ! PoisonPill
            break
          case _ =>
            clientActorRef ! TextLineMsg(line)
        }
      }
    }

    println("Freedom!")
    system.terminate()
  }
}