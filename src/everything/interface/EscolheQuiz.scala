package interface

import funcional.User
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, Separator}
import javafx.scene.layout.VBox
import javafx.scene.text.Text

class EscolheQuiz extends MainPage {
  @FXML
  private var username : Button = _

  @FXML
  private var mainbox : VBox = _

  override def initData(u:User):Unit = {
    user = u
    username.setText(user.nome)

    user.podeJogar() map (x=> {
      val nome = new Label(x.titulo + " de " + x.dono)
      nome.setStyle("-fx-font-size: 24")
      nome.setWrapText(true)
      mainbox.getChildren.add(nome)

      val descricao = new Text(x.descricao)
      mainbox.getChildren.add(descricao)

      val nperguntas = new Label("Este quiz tem " + x.perguntas.length + " perguntas")
      mainbox.getChildren.add(nperguntas)

      lazy val tentativas = x.percentagemCorretas filter(x=>x._1.equals(user.nome))
      lazy val ntentativas = tentativas.length

      if(ntentativas == 0) mainbox.getChildren.add(new Label("Nunca jogou a este quiz"))
      else mainbox.getChildren.add(new Label("Jogou este quiz " + ntentativas + " vezes, e a melhor pontuação que teve foi " + (tentativas map (x=>x._3)).max + "%"))

      val button = new Button("Jogar")
      button.setOnAction(new EventHandler[ActionEvent]() {
        override def handle(e: ActionEvent): Unit = {
          Utils.novaScenaComQuiz(button.getScene, user, x, "jogaquiz.fxml")
        }
      })
      mainbox.getChildren.add(button)

      mainbox.getChildren.add(new Separator())
    })
  }
}
