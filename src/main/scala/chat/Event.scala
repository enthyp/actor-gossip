package chat

/*
* Client-server communication protocol.
* */

sealed trait Event
case class Login(user: String) extends Event
case class Logout(user: String) extends Event
case class GetChatLog(user: String) extends Event
case class ChatLog(log: List[String]) extends Event
case class ChatMessage(from: String, msg: String) extends Event