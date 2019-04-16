package chat.client

import akka.actor.Actor

/*
* Handles single conversation.
* Possibly to be subclassed for text vs audio communication.
* */

class SessionSupervisor extends Actor {
  override def receive: Receive = ???
}
