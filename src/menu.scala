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

  def mainLoop(u: String): Unit = {
    //criar um objecto como o prof fez para se poder fazer um loop em vez de um long String
    val input = getUserInput("1)perfil, 2)tracker, 3)qizzes, 4)compras, 5)alimentacao, 6)outros perfis, 7)logout")
    input match{
      case 1 => println("perfil"); mainLoop(u)
      case 2 => println("tracker"); mainLoop(u)
      case 3 => println("qizzes"); mainLoop(u)
      case 4 => println("compras"); mainLoop(u)
      case 5 => println("alimentacao"); mainLoop(u)
      case 6 => println("outros perfis"); mainLoop(u)
      case 7 => println("logout"); logout()
      case _ => println("Invalid Input"); mainLoop(u)
    }
  }
  mainLoop("ola");
}
