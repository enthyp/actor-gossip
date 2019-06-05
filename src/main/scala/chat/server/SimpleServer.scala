package chat.server

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import chat._
import com.typesafe.config.ConfigFactory

object SimpleServer {
  def props(): Props = Props(new SimpleServer)
}

class SimpleServer extends Actor with ActorLogging {
  override def preStart(): Unit = log.info("Starting server.")
  override def postStop(): Unit = log.info("Server stopped.")

  override def receive: Receive = {
    case Login(user) =>
      println(s"login $user")
    case Logout(user) =>
      println(s"logout $user")
    case GetChatRooms =>
      println("get chat rooms")
    case Join(user, room) =>
      println(s"join $room by $user")
    case Leave(user) =>
      println(s"leave by $user")
    case ChatMessage(from, msg) =>
      println(s"message $msg from $from")
    case other =>
      log.error(s"Unexpected message: $other")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val portConfig = ConfigFactory.load("server")
    val system = ActorSystem("chat-server-system", portConfig)

    val serverActorRef = system.actorOf(SimpleServer.props(), "server")
  }
}