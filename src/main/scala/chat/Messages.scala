package chat

import akka.actor.ActorRef
import chat.server.Room

/*
* Client-server communication protocol.
* */

sealed trait Message

case class RequestLogin(user: String, ref: ActorRef) extends Message
case class ResponseLoggedIn(user: String, remoteActor: ActorRef) extends Message
case class ResponseNameTaken(user: String) extends Message

case class Logout(user: String) extends Message

case object RequestChatRooms extends Message
case class ResponseChatRooms(rooms: List[String]) extends Message

// TODO: add these!
case class RequestCreateRoom(name: String) extends Message
case class ResponseRoomCreated(name: String) extends Message

case class RequestDeleteRoom(name: String) extends Message
case class ResponseRoomDeleted(name: String) extends Message

case class RequestJoin(room: String) extends Message
case class ResponseJoined(room: String) extends Message
case class ResponseNoRoom(room: String) extends Message

case object RequestChatHistory extends Message
case class ResponseChatHistory(history: List[ChatMessage]) extends Message

case class ChatMessage(from: String, msg: String) extends Message with Room.PublishableMessage
case object RequestLeave extends Message
case class ResponseLeft(room: String) extends Message