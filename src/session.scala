import ioUtils._
import users.User

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object session {
  def logout(u:User):Option[User] ={
    writeTrackers(u.trackers)
    None //Dps da validação
  }

  def exit():Option[User] ={
    None //Dps da validação (Faz sentido returnar None quando o User quer sair?)
  }

  @tailrec
  def login():Option[User] = {
    val u = readLine("Username: ").trim
    val p = readLine("Passoword: ").trim

    //ver se há ficheiro com o mmo username, se sim ver se a pass é igual
    readUser(u,p) match {
      case None => println("Os dados submetidos estavam incorretos"); login()
      case x => x //change to
    }
  }

  @tailrec
  def createUser():Option[User] = {
    val username = readLine("Username: ").trim
    val password = readLine("Password: ").trim
    val repeticao = readLine("Repetir Password: ").trim

    //ver se há ficheiro com o mmo username, se ñ e as passes forem iguais cria o ficheiro
    password.compareTo(repeticao) match {
      case 0 => writeUser(username, password) match {
        case None => println("Algum dos dados submetidos esta incorreto"); None //User já existe
        case _ => Some(User(username, password, List()))
      }
      case _ => println("As Passwords submetidas tem de ser iguais")
        createUser()
    }
  }
}
