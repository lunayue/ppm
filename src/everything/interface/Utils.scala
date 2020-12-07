package interface

import funcional.{Quiz, Registo, Tracker, User}
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import javafx.stage.Modality

object Utils {

  def novaScenaComInfo(scene: Scene, user:User, name:String): Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(getClass.getResource(name))
    val root:Parent = loader.load()
    val mpc:PassUser = loader.getController
    mpc.initData(user)
    scene.setRoot(root)
  }

  def novaScenaComTracker(scene: Scene, user:User, tracker:Tracker, name:String): Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(getClass.getResource(name))
    val root:Parent = loader.load()
    val mpc:PassaUserTracker = loader.getController
    mpc.initData(user, tracker)
    scene.setRoot(root)
  }

  def novaScenaComRegisto(scene: Scene, user:User, tracker:Tracker, registo:Registo, name:String): Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(getClass.getResource(name))
    val stage = new Stage()
    stage.setScene(new Scene(loader.load()))
    val mpc:PassaUserTrackerRegisto = loader.getController
    mpc.initData(user, tracker, registo, scene)
    stage.initModality(Modality.APPLICATION_MODAL)
    stage.show()
  }

  def novaScenaComQuiz(scene: Scene, user:User, quiz:Quiz, name:String): Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(getClass.getResource(name))
    val root:Parent = loader.load()
    val mpc:PassaUserQuiz = loader.getController
    mpc.initData(user, quiz)
    scene.setRoot(root)
  }

  def novaScenaResultadoQuiz(scene: Scene, user:User, quiz:Quiz, bs:List[Boolean]): Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(getClass.getResource("vecorrecao.fxml"))
    val root:Parent = loader.load()
    val mpc:VeCorrecao = loader.getController
    mpc.initData(user, quiz, bs)
    scene.setRoot(root)
  }

  def abreMain(scene:Scene, user:User):Unit = novaScenaComInfo(scene, user, "mainpage.fxml")

  def abreVerTrackers(scene:Scene, user:User):Unit = novaScenaComInfo(scene, user, "vertrackers.fxml")
  def abreAdicionarTracker(scene:Scene, user:User):Unit = novaScenaComInfo(scene, user, "adicionartracker.fxml")
  def abreCriarTracker(scene:Scene, user:User):Unit = novaScenaComInfo(scene, user, "criartracker.fxml")

  def abreEscolheQuiz(scene:Scene, user:User):Unit = novaScenaComInfo(scene, user, "escolhequiz.fxml")

  def abreAjuda(scene:Scene, user:User):Unit = novaScenaComInfo(scene, user, "verajuda.fxml")
  def abreSugestoes(scene:Scene, user:User):Unit = novaScenaComInfo(scene, user, "versugestoes.fxml")
  def abreCalculos(scene:Scene, user:User):Unit = novaScenaComInfo(scene, user, "calculosauxiliares.fxml")

  def logout(scene:Scene, user:User):Unit = {
    funcional.Sessao.logout(user)
    scene.getWindow.hide()
  }
}
