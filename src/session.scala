import ioUtils._

import scala.annotation.tailrec

object session {
  @tailrec
  def login(errorMSG: Boolean):Option[User] = {
    if(errorMSG){ println("Algum dos dados submetidos esta incorreto")} //Em caso de algum dado submetido esteje incorreto
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")

    //ver se há ficheiro com o mmo username, se sim ver se a pass é igual
    readUser(username,password) match {  //writerUser valida o User (Option[User])
      case None => login(true)  //User não existe
      case _ => Some(User(username,password))
    }
  }

  def createUser(errorMSG: Boolean):Option[User] = {
    if(errorMSG){ println("Algum dos dados submetidos esta incorreto")} //Em caso de algum dado submetido esteje incorreto
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")
    val repeticao = getUserInputSensitive("Repetir Password")

    //ver se há ficheiro com o mmo username, se ñ e as passes forem iguais cria o ficheiro
    if(password.compareTo(repeticao)!=0){
      println("As Passwords submetidas tem de ser iguais")
      createUser(false)
    }
    writeUser(username,password) match {
      case None => createUser(true)  //User já existe
      case _ => Some(User(username,password))
    }
  }


  def logout():Option[User] ={
    //save para file???
    None //Dps da validação
  }

  def exit():Option[User] ={
    println("Adeus!")
    None //Dps da validação (Faz sentido returnar None quando o User quer sair?)

  }
}
