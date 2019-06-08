package chat.client

object Parser {

  sealed trait Command
  final case class CreateRoomCmd(room: String) extends Command
  final case class DeleteRoomCmd(room: String) extends Command
  final case class JoinCmd(room: String) extends Command
  final case object GetRoomsCmd extends Command
  final case object LeaveCmd extends Command
  final case object LogoutCmd extends Command
  final case class UnknownCmd(command: String) extends Command

  final case class ParsingError(private val message: String = "",
                                private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  def parseLine(line: String): Either[Command, String] = {
    val trimmed = line.trim()

    trimmed.split(" ", 2) match {
      case Array() =>
        Right("")
      case Array(head) =>
        if (head.take(1) == "\\")
          Left(parseCommand(head.drop(1), None))
        else
          Right(head)
      case Array(head, rem) =>
        if (head.take(1) == "\\")
          Left(parseCommand(head.drop(1), Some(rem)))
        else
          Right(trimmed)
    }
  }

  def parseCommand(command: String, rem: Option[String]): Command = {
    command match {
      case "rooms" =>
        if (rem.isDefined)
          throw ParsingError(message = "Command: \\rooms takes no parameters.")
        else
          GetRoomsCmd
      case "create" =>
        if (rem.isEmpty || rem.get.split(" ").length > 1)
          throw ParsingError(message = "Command: \\create takes exactly one parameter.")
        else if (!rem.get.forall(_.isLetter))
          throw ParsingError(message = "Room name can contain letters only!")
        else
          CreateRoomCmd(rem.get)
      case "delete" =>
        if (rem.isEmpty || rem.get.split(" ").length > 1)
          throw ParsingError(message = "Command: \\delete takes exactly one parameter.")
        else
          DeleteRoomCmd(rem.get)
      case "join" =>
        if (rem.isEmpty || rem.get.split(" ").length > 1)
          throw ParsingError(message = "Command: \\join takes exactly one parameter.")
        else
          JoinCmd(rem.get)
      case "leave" =>
        if (rem.isDefined)
          throw ParsingError(message = "Command: \\leave takes no parameters.")
        else
          LeaveCmd
      case "logout" =>
        if (rem.isDefined)
          throw ParsingError(message = "Command: \\logout takes no parameters.")
        else
          LogoutCmd
      case other =>
        UnknownCmd(other)
    }
  }
}