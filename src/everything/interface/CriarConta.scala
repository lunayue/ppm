package interface

import funcional.Sessao
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label, PasswordField, TextField}

class CriarConta {
  @FXML
  private var cancelar: Button = _
  @FXML
  private var criarconta: Button = _

  @FXML
  private var username: TextField = _

  @FXML
  private var password1: PasswordField = _
  @FXML
  private var password2: PasswordField = _

  @FXML
  private var emptyfields: Label = _
  @FXML
  private var passwordsdontmatch: Label = _
  @FXML
  private var usernametaken: Label = _

  def onCancelarClicked(): Unit = {
    cancelar.getScene.setRoot(new FXMLLoader(getClass.getResource("loginpage.fxml")).load())
  }

  def onCriarContaClicked(): Unit = {
    lazy val txtp1 = password1.getText
    lazy val txtp2 = password2.getText
    lazy val txtun = username.getText

    if(txtp1.length==0 || txtp2.length==0 || txtun.length==0){
      emptyfields.setVisible(true)
      passwordsdontmatch.setVisible(true)
      usernametaken.setVisible(false)
    }
    else if(!Sessao.passIguais(txtp1, txtp2)) {
      emptyfields.setVisible(false)
      passwordsdontmatch.setVisible(true)
      usernametaken.setVisible(false)
    }
    else{
      emptyfields.setVisible(false)
      passwordsdontmatch.setVisible(false)
      Sessao.createUser(FxApp.users, username.getText, password1.getText) match {
        case Some(x) => Utils.abreMain(criarconta.getScene, x)
        case _ => usernametaken.setVisible(true)
      }
    }
  }
}

