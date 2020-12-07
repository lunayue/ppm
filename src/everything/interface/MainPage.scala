package interface

import funcional.User
import javafx.fxml.FXML
import javafx.scene.control.Button

class MainPage() extends PassUser{
  @FXML
  private var username : Button = _

  var user:User = _

  def initData(u:User):Unit = {
    user = u
    username.setText(user.nome)
  }

  //Trackers
  def vertrackers():Unit = Utils.abreVerTrackers(username.getScene, user)
  def criartracker():Unit = Utils.abreCriarTracker(username.getScene, user)
  def adicionartracker():Unit = Utils.abreAdicionarTracker(username.getScene, user)

  //Quizzes
  def openEscolheQuiz():Unit = Utils.abreEscolheQuiz(username.getScene, user)

  //Ajuda
  def openAjuda():Unit = Utils.abreAjuda(username.getScene, user)
  def openSugestoes():Unit = Utils.abreSugestoes(username.getScene, user)
  def openCalculos():Unit = Utils.abreCalculos(username.getScene, user)

  //butoes direitos
  def openPerfil():Unit = println("perfil")
  def openComparar():Unit = println("comparar")
  def logoutUser():Unit = Utils.logout(username.getScene, user)
}

