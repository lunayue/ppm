import ioUtils._

object session {
  def login() = {
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")
    //ver se há ficheiro com o mmo username, se sim ver se a pass é igual
    //manda Some(User)
  }

  def createUser() = {
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")
    val repeticao = getUserInputSensitive("Repetir Password")
    //ver se há ficheiro com o mmo username, se ñ e as passes forem iguais cria o ficheiro
    //manda Some(User)
  }

  def logout() ={
    //save para file???
  }

  def exit() ={
    println("Adeus!")
  }


}
