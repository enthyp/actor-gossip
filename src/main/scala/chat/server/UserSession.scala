package chat.server

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object UserSession {
  def props(server: ActorRef): Props = Props(new UserSession() {})
}

class UserSession(val server: ActorRef) extends Actor
  with ActorLogging
  with SessionManager
  with ChatManager {

  override def receive: Receive =
    manageSession orElse manageChat orElse unknown

  def unknown: Receive = {
    case other =>
      log.error(s"Unexpected message: $other")
  }
}
