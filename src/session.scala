import scala.annotation.tailrec
import java.io.File
import scala.io._
import ioUtils._

object session {

  val userFileReader:Map[Int,String] = Map(
    1 -> "Username",
    2 -> "Password",
    3 -> "Quizz"
  )

  @tailrec
  def login(errorMSG: Boolean):Option[User] = {
    if(errorMSG){ println("Algum dos dados submetidos esta incorreto")} //Em caso de algum dado submetido esteje incorreto
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")

    //ver se há ficheiro com o mmo username, se sim ver se a pass é igual
    readUser(username,password) match {  //readUser valida o User (Option[User])
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
    None //Faz sentido returnar None quando o User quer sair?
  }

  def userPerfil(user: User): Unit ={
    val input = getUserInput("1)Ver Perfil, 2)Editar Perfil")
    input match {
      case "1" => println("O seu Perfil"); verPerfil(user)
      case "2" => println("Editar Perfil"); editarPerfile(user)
      case "q" | "Q" =>
      case _ => println("Invalid Input"); userPerfil(user)
    }
  }

  def verPerfil(user: User): Unit ={
    val bufferedSource = Source.fromFile("src" + File.separator +"users" + File.separator + user.username +".txt")
    var i = 1
    for (line <- bufferedSource.getLines){
      println(userFileReader(i) + ": " + line)
      i = math.min(i + 1, userFileReader.size)
    }
    bufferedSource.close
  }

  def editarPerfile(user: User): Unit = {
    //Perguntar se que mudar de username ou Password
  }
}