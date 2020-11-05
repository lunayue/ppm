import ioUtils._

object session {
  def login() = {
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")
  }

  def createUser() = {
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")
    val repeticao = getUserInputSensitive("Repetir Password")
  }

  def logout() ={

  }

  def exit() ={
    println("Adeus!")
  }


}
