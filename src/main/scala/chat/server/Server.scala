package chat.server

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import chat._
import com.typesafe.config.ConfigFactory

import scala.collection.mutable

object Server {
  def props(): Props = Props(new Server)

  // server internals
  sealed trait ServerMessage

}

class Server extends Actor with ActorLogging {
  override def preStart(): Unit = log.info("Starting server.")
  override def postStop(): Unit = log.info("Server stopped.")

  val sessions = new mutable.HashMap[String, ActorRef]
  val rooms = new mutable.HashMap[String, ActorRef]
  rooms += ("bob" -> context.actorOf(Room.props("bob"), "bob_room"))
  rooms += ("pyp" -> context.actorOf(Room.props("pyp"), "pyp_room"))

  override def receive: Receive = {
    case Login(user) =>
      log.info(s"LOGIN: $user")

      if (sessions.contains(user))
        sender() ! NameTaken(user)
      else {

        val newSession = context.actorOf(UserSession.props(self), s"session_$user")
        sessions += (user -> newSession)
        sender() ! LoggedIn(user, newSession)
      }

    case other =>
      log.error(s"Unexpected message: $other")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val portConfig = ConfigFactory.load("server")
    val system = ActorSystem("chat-server-system", portConfig)

    val serverActorRef = system.actorOf(Server.props(), "server")
  }
}