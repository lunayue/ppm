package interface

import funcional.User
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, Separator}
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.layout.VBox
import javafx.scene.text.Text

class AdicionarTracker extends MainPage {
  @FXML
  private var username : Button = _

  @FXML
  private var mainbox : VBox = _

  override def initData(u:User):Unit = {
    user = u
    username.setText(user.nome)

    user.podeAdicionarTracker(FxApp.users.values.toList) map (x=> {
      val nome = new Label(x._2.nome + " de " + x._1)
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
      val button = new Button("Adicionar este Tracker")

      button.setOnAction(new EventHandler[ActionEvent]() {
        override def handle(e: ActionEvent): Unit = {
          Utils.abreAdicionarTracker(button.getScene, user.adicionaTracker(x._2)._1)
        }
      })
      mainbox.getChildren.add(button)
      mainbox.getChildren.add(new Separator())
    })
  }
}
