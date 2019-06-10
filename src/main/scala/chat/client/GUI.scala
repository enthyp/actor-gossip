package chat.client

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import com.typesafe.config.ConfigFactory
import scalafx.Includes._
import scalafx.application.{JFXApp, Platform}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout._
import scalafx.scene.paint.Color._
import scalafx.stage.{Modality, Stage, WindowEvent}

object UIActor {
  def props: Props = Props(new UIActor)

  // UI messages
  sealed trait UIMessage
  sealed trait Dialogable {
    def header: String
    def content: String
  }
  case class RequestConnect(host: String, port: Int, user: String) extends UIMessage
  case class ResponseError(reason: String, text: String) extends UIMessage with Dialogable {
    override def header: String = reason
    override def content: String = text
  }
  case object ResponseConnected extends UIMessage

  case class InputLine(line: String) extends UIMessage

  case object Disconnect extends UIMessage
}

class UIActor extends Actor with ActorLogging {

  var clientActor: Option[ActorRef] = None

  import UIActor._

  def receive: Receive = {
    case msg =>
      context.become(initial)
      self ! msg
  }

  def initial: Receive = {
    case RequestConnect(host, port, user) =>
      val serverActorRef =
        context.actorSelection(s"akka.tcp://chat-server-system@$host:$port/user/server")
      if (clientActor.isDefined) {
        context.stop(clientActor.get)
      }

      clientActor = Some(context.actorOf(Client.props(self, serverActorRef), s"client_$host:$port"))
      clientActor.get ! Client.InputLineMsg(user)

    case ResponseConnected =>
      context.become(connected)
      Platform.runLater {
        UI.stateMain()
      }

    case msg: ResponseError =>
      Platform.runLater {
        UI.connectionFailure(msg)
      }

    case Disconnect =>
      if (clientActor.isDefined)
        clientActor.get ! chat.Logout
  }

  def connected: Receive = {
    case UIActor.InputLine(line) =>
      clientActor.get ! Client.InputLineMsg(line)

    case Client.ChatRoomsMsg(rooms) =>
      Platform.runLater {
        UI.setRooms(rooms)
      }

    case Client.JoinedMsg(room) =>
      Platform.runLater {
        UI.stateConversation(room)
      }

    case Client.LeftMsg =>
      Platform.runLater {
        UI.closeChatWindow()
      }

    case Client.ChatMsg(line) =>
      Platform.runLater {
        UI.addMsg(line)
      }

    case Client.LoggedOutMsg =>
      Platform.runLater {
        UI.logout()
      }

    case msg: ResponseError =>
      Platform.runLater {
        UI.connectionFailure(msg)
      }
  }
}

object UI extends JFXApp {

  val system = ActorSystem("chat-client-system", ConfigFactory.load("client"))
  val uiActor = system.actorOf(UIActor.props, "UI")

  val roomsList: ObservableBuffer[String] = new ObservableBuffer[String]
  val msgList: ObservableBuffer[String] = new ObservableBuffer[String]
  var convStage: Option[Stage] = None

  stage = new JFXApp.PrimaryStage {
    title.value = "Server choice"

    scene = new Scene {
      fill = LightGrey
      root = connectWindow

      onShown = _ => {
        minWidth = 300
        minHeight = 200
      }
    }
  }

  def connectWindow: GridPane = {
    val inputGrid = new GridPane {
      alignment = Pos.TopCenter
      hgap = 10
      vgap = 10
      padding = Insets(10, 10, 10, 10)
    }

    inputGrid.add(new Label("Server IP"), 0, 0)
    val hostField = new TextField { text = "127.0.0.1" }
    inputGrid.add(hostField, 1, 0)

    inputGrid.add(new Label("Server port"), 0, 1)
    val portField = new TextField { text = "2551" }
    inputGrid.add(portField, 1, 1)

    inputGrid.add(new Label("Nickname"), 0, 2)
    val nickField = new TextField
    inputGrid.add(nickField, 1, 2)

    val column1 = new ColumnConstraints()
    column1.setPercentWidth(30)
    val column2 = new ColumnConstraints()
    column2.setPercentWidth(70)
    inputGrid.columnConstraints = List(column1, column2)

    val connectButton = new Button {
      text = "Connect"
      onAction = _ => {
        uiActor ! UIActor.RequestConnect(
          hostField.text.value,
          portField.text.value.toInt,
          nickField.text.value
        )
      }
    }

    val buttonRow = new HBox {
      children = List(connectButton)
      alignment = Pos.BottomRight
    }

    inputGrid.add(buttonRow, 1, 3)
    inputGrid
  }

  def connectionFailure(error: UIActor.ResponseError): Unit = {
    dialog(error).showAndWait()
  }

  def dialog[A <: UIActor.Dialogable](msg: A): Dialog[A] = {
    val dialog = new Dialog[A] {
      title = msg.header
      contentText = msg.content
    }
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK)
    dialog
  }

  def stateMain(): Unit = {
    stage.setHeight(500)
    stage.getScene.setRoot(mainWindow)
    stage.title = "Chat"
  }

  def mainWindow: BorderPane = {
    val mainPanel = new BorderPane() {

    }

    val cp = centerPanel
    cp.margin = Insets(10, 5, 5, 5)
    mainPanel.setCenter(cp)

    val ip = inputPanel
    ip.margin = Insets(0, 5, 5, 5)
    mainPanel.setBottom(ip)
    mainPanel
  }

  def centerPanel: StackPane = {
    val lv = new ListView[String](roomsList)
    lv.setMouseTransparent(true)
    lv.setFocusTraversable(false)

    lv.getItems.onChange((_, _) => {
      lv.setItems(roomsList)
    })

    lv.cellFactory = (l: ListView[String]) => {
      val cell = new ListCell[String]()
      cell.item.onChange { (_, _, str) =>
        cell.text = str
        cell.setMouseTransparent(true)
        cell.setFocusTraversable(false)
        cell.setWrapText(true)
        cell.prefWidth = 50
      }
      cell
    }

    lv.placeholder = new ListView[String](
      new ObservableBuffer[String] ++= List("No rooms loaded.")
    )

    val sp = new StackPane {
      children = lv
    }
    sp
  }

  def inputPanel: TextField = {
    val input = new TextField {
      onKeyPressed = (event: KeyEvent) => {
        if (event.code.equals(KeyCode.Enter)) {
          uiActor ! UIActor.InputLine(this.text.value)
          this.text = ""
        }
      }
    }

    input
  }

  def setRooms(rooms: List[String]): Unit = {
    roomsList.clear()
    roomsList.insert(0, rooms :_*)
  }

  def stateConversation(room: String): Unit = {
    val newStage = new Stage {
      scene = new Scene {
        title.value = room
        fill = LightGrey
        root = chatWindow

        onShown = _ => {
          minWidth = 300
          minHeight = 400
        }
      }
    }

    convStage = Some(newStage)

    newStage.onCloseRequest = _ => {
      uiActor ! UIActor.InputLine("\\leave")
    }

    newStage.initModality(Modality.ApplicationModal)
    newStage.showAndWait()
  }

  def chatWindow: BorderPane = {
    val chatPanel = new BorderPane() {

    }

    val cp = convPanel
    cp.margin = Insets(10, 5, 5, 5)
    chatPanel.setCenter(cp)

    val ip = inputPanel
    ip.margin = Insets(0, 5, 5, 5)
    chatPanel.setBottom(ip)
    chatPanel
  }

  def convPanel: StackPane = {
    val lv = new ListView[String](msgList)

    lv.getItems.onChange((_, _) => {
      lv.setItems(msgList)
    })

    lv.cellFactory = (l: ListView[String]) => {
      val cell = new ListCell[String]()
      cell.item.onChange { (_, _, str) =>
        cell.text = str
        cell.setMouseTransparent(true)
        cell.setFocusTraversable(false)
        cell.setWrapText(true)
        cell.prefWidth = 50
      }
      cell
    }

    val sp = new StackPane {
      children = lv
    }
    sp
  }

  def addMsg(msg: String): Unit = {
    msgList += msg
  }

  def closeChatWindow(): Unit = {
    msgList.clear()
    if (convStage.isDefined)
      convStage.get.close()
  }

  def logout(): Unit = {
    stage.fireEvent(new WindowEvent(stage, WindowEvent.WindowCloseRequest))
  }

  override def stopApp(): Unit = {
    uiActor ! UIActor.Disconnect
    system.terminate()
  }
}

class DeadCell extends ListCell[String] {
  private val label = new Label()
  private val pane: StackPane = new StackPane()
  pane.minWidth = 0
  pane.prefWidth = 1
  pane.children += label

  this.item.onChange { (_, _, str) =>
    this.text = str
    this.setMouseTransparent(true)
    this.setFocusTraversable(false)
  }
}