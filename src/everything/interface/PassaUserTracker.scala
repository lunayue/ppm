package interface

import funcional.{Tracker, User}

abstract class PassaUserTracker extends MainPage {
  def initData(user:User, tracker:Tracker):Unit
}
