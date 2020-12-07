package interface

import funcional.User
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, Separator}
import javafx.scene.layout.VBox

class VerAjuda extends MainPage {
  @FXML
  private var username : Button = _

  @FXML
  private var mainbox : VBox = _

  override def initData(u:User):Unit = {
    user = u
    username.setText(user.nome)

    FxApp.ajudas map (x=> {
      val p = new Label(x.pergunta)
      p.setStyle("-fx-font-size: 24")
      p.setWrapText(true)
      mainbox.getChildren.add(p)
      val r = new Label(x.resposta)
      r.setWrapText(true)
      mainbox.getChildren.add(r)
      mainbox.getChildren.add(new Separator())
    })
  }
}
