package interface
import funcional.Utils.makeInt
import funcional.{Registo, Tracker, User}
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXML
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.{Alert, Button, ButtonType, Label, TextField}
import javafx.scene.layout.GridPane

import scala.annotation.tailrec

class VerTrackerEspecifico extends PassaUserTracker {
  @FXML
  private var username : Button = _

  @FXML
  private var nometracker : Label = _
  @FXML
  private var descricao : Label = _
  @FXML
  private var camposvazios : Label = _
  @FXML
  private var nomeinvalido : Label = _
  @FXML
  private var dadoinvalido : Label = _

  @FXML
  private var nome : TextField = _
  @FXML
  private var dado : TextField = _

  @FXML
  private var grid : GridPane = _

  var tracker:Tracker = _

  def initData(u:User, t:Tracker):Unit = {
    user = u
    tracker = t
    username.setText(user.nome)
    nometracker.setText(tracker.nome)
    descricao.setText(tracker.descricao)

    @tailrec
    def loop(i:Int, rs:List[Registo]):(Int,List[Registo]) = rs match {
      case Nil => (i, List())
      case a::tail =>{
        grid.add(new Label(a.data), 0, i)
        grid.add(new Label(a.dado.toString), 1, i)
        grid.add(new Label(a.meta.toString), 2, i)
        if(a.melhorUltrapassar){grid.add(new Label("Sim"), 3, i)} else {grid.add(new Label("Nao"), 3, i)}
        if(a.alcancado()){grid.add(new Label("Sim"), 4, i)} else {grid.add(new Label("Nao"), 4, i)}

        val button1 = new Button("Editar")
        button1.setOnAction(new EventHandler[ActionEvent]() {
          override def handle(e: ActionEvent): Unit = {
            Utils.novaScenaComRegisto(button1.getScene, user, tracker, a, "editarregisto.fxml")
          }
        })
        grid.add(button1, 5, i)

        val button2 = new Button("Apagar")
        button2.setOnAction(new EventHandler[ActionEvent]() {
          override def handle(e: ActionEvent): Unit = {
            val alert = new Alert(AlertType.CONFIRMATION)
            alert.setTitle("Apagar Registo")
            alert.setHeaderText("Tem a certeza?")
            alert.setContentText("Após apagar não será capaz de recuperar o registo")

            val result = alert.showAndWait
            if (result.get eq ButtonType.OK) {
              val novoT = tracker.removeRegisto(a.data)._1
              Utils.novaScenaComTracker(button2.getScene,user.alteraTracker(novoT)._1, novoT, "vertrackerespecifico.fxml")
            }
          }
        })
        grid.add(button2, 6, i)
        loop(i+1, tail)
      }
    }
    loop(1, tracker.registos)
  }

  def adiciona():Unit = {
    val n = nome.getText
    val d = dado.getText
    val di = makeInt(d)
    if(n.length == 0 || d.length == 0){
      camposvazios.setVisible(true)
      nomeinvalido.setVisible(false)
      dadoinvalido.setVisible(false)
    }
    else if(di.isFailure || di.get < 0){
      camposvazios.setVisible(false)
      nomeinvalido.setVisible(false)
      dadoinvalido.setVisible(true)
    }
    else{
      tracker.adicionarRegisto(n, di.get) match {
        case (nt, Some(_)) =>  Utils.novaScenaComTracker(username.getScene, user.alteraTracker(nt)._1, nt, "vertrackerespecifico.fxml")
        case (_, None) => camposvazios.setVisible(false)
          nomeinvalido.setVisible(true)
          dadoinvalido.setVisible(false)
      }

    }

  }
}
