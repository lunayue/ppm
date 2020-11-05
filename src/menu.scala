import ioUtils._
import session._

object menu extends App{
  def loginLoop(): Unit ={
    val input = getUserInput("(l)ogin, (c)reate user, (q)uit:")
    input match{
      case "L" => login() //case None => loginLoop()
      case "C" => createUser() //case None => loginLoop()
      case "Q" => exit()
      case _ => println("Invalid Input"); loginLoop()
    }
  }
  loginLoop()

  def mainLoop(u: String) = {
    //criar um objecto como o prof fez para se poder fazer um loop em vez de um long String
    val input = getUserInput("(l)ogin, (c)reate user, logout")
    input match{
      case "L" => login()
      case "C" => createUser()
      case "Q" => logout()
      case _ => println("Invalid Input"); loginLoop()
    }
  }
}
