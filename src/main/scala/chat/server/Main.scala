package chat.server

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

object Main {
  def main(args: Array[String]): Unit = {

    val usage =
      """
        | usage: run [-lh seed_host] [-lp seed_port] [-sh server_host] [-sp server_port]
        | By default server is run on 127.0.0.1:2552, trying to link to local cluster seed
        | on 127.0.0.1:2551
      """.stripMargin

    var seedHost = "127.0.0.1"
    var seedPort = 2551
    var serverHost = seedHost
    var serverPort = 2552

    if (args.length > 0) {
      val argsList = args.toList

      def parseOptions(args: List[String]): Unit = {
        args match {
          case Nil =>
          case "-lh" :: host :: tail =>
            seedHost = host
            parseOptions(tail)
          case "-lp" :: port :: tail =>
            seedPort = port.toInt
            parseOptions(tail)
          case "-sh" :: host :: tail =>
            serverHost = host
            parseOptions(tail)
          case "-sp" :: port :: tail =>
            serverPort = port.toInt
            parseOptions(tail)
          case opt =>
            println(s"Unknown option: $opt")
            println(usage)
            System.exit(1)
        }
      }

      try {
        parseOptions(argsList)
      } catch {
        case _: NumberFormatException =>
          println("Port numbers must be integers.")
          println(usage)
          System.exit(1)
        case e: Throwable =>
          println(s"Error occurred: $e")
          println(usage)
          System.exit(1)
      }
    }

    val serverConfig = ConfigFactory.parseString(
      s"""
         |akka.remote.netty.tcp.hostname="$serverHost"
         |akka.remote.netty.tcp.port=$serverPort
         |akka.cluster.seed-nodes=["akka.tcp://chat-server-system@${seedHost}:${seedPort}"]
       """.stripMargin)
      .withFallback(ConfigFactory.load("server"))

    val system = ActorSystem("chat-server-system", serverConfig)
    val serverActorRef = system.actorOf(Server.props(), "server")

    // When done.
    scala.io.StdIn.readLine()
    system.terminate()
  }
}