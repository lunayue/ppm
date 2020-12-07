package interface

import funcional.{Quiz, User}

abstract class PassaUserQuiz extends MainPage {
  def initData(user:User, quiz:Quiz):Unit
}
