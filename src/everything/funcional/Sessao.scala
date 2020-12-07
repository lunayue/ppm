
package funcional

import funcional.Utils.{readQuizzes, readSugestoes, writeQuizzes, writeSugestao, writeUser}

import scala.util.{Failure, Success, Try}

object Sessao {
  def login(users:Map[String,User], username:String, password:String):Option[User] = {
    existsUser(users, username) match {
      case Some(x) =>if(x.password.equals(password)) Some(x) else None
      case None => None
    }
  }

  def existsUser(users:Map[String,User], username:String):Option[User] = {
    Try(users(username)) match {
      case Success(x) => Some(x)
      case Failure(_) => None
    }
  }

  //Alterações
  def passIguais(a:String, b:String):Boolean = a.compareTo(b) == 0

  def createUser(users:Map[String,User], username:String, password:String):Option[User] = {
    existsUser(users, username) match {
      case None => Option(User(username, password, Map(), readSugestoes(), readQuizzes()))
      case _ => None
    }
  }
  //Acabaram as alterações

  def logout(u:User):User = {
    writeSugestao(u.sugestoes)
    writeQuizzes(u.quizzes.values.toList)
    writeUser(u)
  }
}