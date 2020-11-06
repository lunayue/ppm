import ioUtils._

object session {
  def login():Option[User] = {
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")
    //ver se há ficheiro com o mmo username, se sim ver se a pass é igual
    Some(User(username,password)) //Dps da validação
  }

  def createUser():Option[User] = {
    val username = getUserInputSensitive("Username")
    val password = getUserInputSensitive("Password")
    val repeticao = getUserInputSensitive("Repetir Password")
    //ver se há ficheiro com o mmo username, se ñ e as passes forem iguais cria o ficheiro
    Some(User(username,password)) //Dps da validação
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
