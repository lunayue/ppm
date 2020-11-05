import ioUtils._

object session {
  def login() = {
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")
    //manda Some(User)
  }

  def createUser() = {
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")
    val repeticao = getUserInputSensitive("Repetir Password")
    //manda Some(User)
  }

  def logout() ={
    //save para file???
  }

  def exit() ={
    println("Adeus!")
  }


}
