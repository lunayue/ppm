package interface

import funcional.{Registo, Tracker, User}
import javafx.scene.Scene

abstract class PassaUserTrackerRegisto {
  def initData(user:User, tracker:Tracker, registo:Registo, scene:Scene):Unit
}
