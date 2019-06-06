package chat.server

import akka.actor.{Actor, ActorRef}
import chat.{ChatMessage, Leave}

trait ChatManager { this: Actor =>

  val server: ActorRef

  protected def manageChat: Receive = {
    case Leave(user) =>
      println(s"leave by $user")
    case ChatMessage(from, msg) =>
      println(s"message $msg from $from")
  }
}
