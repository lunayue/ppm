package interface

import funcional.Utils.{readAjudas, readUsers}
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.{Stage, StageStyle}

class HabitTracker extends Application {
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Habit Tracker")
    //primaryStage.initStyle(StageStyle.UNDECORATED)
    val fxmlLoader =
      new FXMLLoader(getClass.getResource("loginpage.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot,800,300)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}
object FxApp {
  lazy val users = readUsers()
  lazy val ajudas = readAjudas()
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[HabitTracker], args: _*)
  }
}
