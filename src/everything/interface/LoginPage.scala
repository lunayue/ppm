package interface

import funcional.Sessao.login
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label, PasswordField, TextField}

class LoginPage {
  @FXML
  private var loginb: Button = _
  @FXML
  private var criarconta: Button = _

  @FXML
  private var username: TextField = _
  @FXML
  private var password: PasswordField = _

  @FXML
  private var wronginput: Label = _

  def onLoginClicked(): Unit = {
    login(FxApp.users, username.getText, password.getText) match {
      case Some(x) => Utils.abreMain(loginb.getScene, x)
      case _ =>  wronginput.setVisible(true)
    }
  }

  def onCriarContaClicked(): Unit = {
    loginb.getScene.setRoot(new FXMLLoader(getClass.getResource("criarconta.fxml")).load())
  }

  def onclicksair():Unit = {
    loginb.getScene.getWindow.hide
  }
}