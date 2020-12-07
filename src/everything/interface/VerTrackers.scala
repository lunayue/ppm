package interface

import funcional.{Calculos, User}
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, Separator}
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.control.Alert
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.ButtonType
import javafx.scene.text.Text

class VerTrackers extends MainPage {
  @FXML
  private var username : Button = _

  @FXML
  private var mainbox : VBox = _

  override def initData(u:User):Unit = {
    user = u
    username.setText(user.nome)

    user.trackers map (x=> {
      val nome = new Label(x._2.nome)
      nome.setStyle("-fx-font-size: 24")
      nome.setWrapText(true)
      mainbox.getChildren.add(nome)

      val descricao = new Text(x._2.descricao)
      mainbox.getChildren.add(descricao)

      if(x._2.melhorUltrapassar){
        mainbox.getChildren.add(new Label("O objetico é ultrapassar " + x._2.meta.toString))
      }
      else{
        mainbox.getChildren.add(new Label("O objetico é ficar abaixo de " + x._2.meta.toString))
      }

      mainbox.getChildren.add(new Label("Efetuou " + x._2.nTotal + " registos com " + x._2.percentagemAlcancado + "% de sucesso"))
      mainbox.getChildren.add(new Label("Somando todos os registos, teve um total de " + Calculos.total(x._2) + " ocurrencias"))
      mainbox.getChildren.add(new Label("Se tivesse atigindo a meta em todos os registos teria mais " + Calculos.poupanca(x._2) + " ocurrencias"))
      mainbox.getChildren.add(new Label("Teve mais " + Calculos.ganho(x._2) + " ocurrencias do que se tivesse apenas atingido a meta em todos os registos"))

      val hb = new HBox()
      val button1 = new Button("Editar")
      button1.setOnAction(new EventHandler[ActionEvent]() {
        override def handle(e: ActionEvent): Unit = {
          Utils.novaScenaComTracker(button1.getScene, user, x._2, "editartracker.fxml")
        }
      })
      hb.getChildren.add(button1)

      val button2 = new Button("Registos")
      button2.setOnAction(new EventHandler[ActionEvent]() {
        override def handle(e: ActionEvent): Unit = {
          Utils.novaScenaComTracker(button2.getScene, user, x._2, "vertrackerespecifico.fxml")
        }
      })
      hb.getChildren.add(button2)

      val button3 = new Button("Apagar")
      button3.setOnAction(new EventHandler[ActionEvent]() {
        override def handle(e: ActionEvent): Unit = {
          val alert = new Alert(AlertType.CONFIRMATION)
          alert.setTitle("Apagar Tracker")
          alert.setHeaderText("Tem a certeza?")
          alert.setContentText("Após apagar não será capaz de recuperar o tracker")

          val result = alert.showAndWait
          if (result.get eq ButtonType.OK) {
            Utils.abreVerTrackers(button3.getScene,user.removeTracker(x._2)._1)
          }
        }
      })
      hb.getChildren.add(button3)
      mainbox.getChildren.add(hb)

      mainbox.getChildren.add(new Separator())
    })
  }
}
