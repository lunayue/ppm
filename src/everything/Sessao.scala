package everything

import everything.Utils.{readQuizzes, writeQuizzes, writeSugestao, writeUser}

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

  def createUser(users:Map[String,User], username:String, password:String, repeticao:String):Option[User] = {
    println(password + "->" + repeticao)
    existsUser(users, username) match {
      case None => if(password.compareTo(repeticao) == 0) Option(User(username, password, Map(), List(), readQuizzes())) else {println("As Passwords submetidas tem de ser iguais")
        None}
      case _ => println("Utilizador já existente")
        None
    }
  }

  def logout(u:User):User = {
    writeSugestao(u.sugestoes)
    writeQuizzes(u.quizzes.values.toList)
    writeUser(u)
  }
}
