package interface

import funcional.{Sugestao, User}
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextArea, TextField}
import javafx.scene.layout.VBox
import javafx.scene.text.Text

class VerSugestoes extends MainPage {
  @FXML
  private var username : Button = _

  @FXML
  private var mainbox : VBox = _

  @FXML
  private var titulosugestao : TextField = _

  @FXML
  private var textosugestao : TextArea = _

  @FXML
  private var wronginputs : Label = _

  override def initData(u:User):Unit = {
    user = u
    username.setText(user.nome)

    user.sugestoes map (x=> {
      val titulo = new Label(x.titulo)
      titulo.setStyle("-fx-font-size: 24")
      titulo.setWrapText(true)
      mainbox.getChildren.add(titulo)
      val texto = new Text(x.texto)
      mainbox.getChildren.add(texto)
      val escritor = new Label("Esta sugestão foi escrita por " + x.user)
      escritor.setStyle("-fx-font-style: italic")
      mainbox.getChildren.add(escritor)
    })
  }

  def submeterSugestao():Unit = {
    lazy val t1 = titulosugestao.getText
    lazy val t2 = textosugestao.getText
    if(t1.length==0 || t2.length == 0)
      wronginputs.setVisible(true)
    else{
      Utils.abreSugestoes(titulosugestao.getScene,user.daSugestao(Sugestao(user.nome, t1, t2)))
    }
  }
}
