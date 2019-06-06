package chat

import akka.actor.ActorRef

/*
* Client-server communication protocol.
* */

sealed trait Message

case class Login(user: String) extends Message
case class LoggedIn(user: String, remoteActor: ActorRef) extends Message
case class NameTaken(user: String) extends Message

case class Logout(user: String) extends Message

case class GetChatRooms(user: String) extends Message
case class ChatRooms(rooms: List[String]) extends Message

case class Join(user: String, room: String) extends Message
case class Joined(room: String) extends Message
case class ChatLog(log: List[ChatMessage]) extends Message
case class ChatMessage(from: String, msg: String) extends Message
case class Leave(user: String) extends Message
case class LeftRoom(room: String) extends Message
