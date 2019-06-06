package chat

import akka.actor.ActorRef

/*
* Client-server communication protocol.
* */

sealed trait Message

case class Login(user: String) extends Message
case class LoggedIn(user: String, remoteActor: ActorRef) extends Message
case class NameTaken(user: String) extends Message

case object RequestLogout extends Message

case object RequestChatRooms extends Message
case class RespondChatRooms(rooms: List[String]) extends Message

case class Join(user: String, room: String) extends Message
case class Joined(room: String) extends Message
case class ChatLog(log: List[ChatMessage]) extends Message
case class ChatMessage(from: String, msg: String) extends Message
case object Leave extends Message
case object LeftRoom extends Message
