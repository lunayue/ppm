import ioUtils._
import session._

import scala.io.StdIn.readLine

object menu extends App{
  def loginLoop(): Unit ={
    val input = getUserInput("(l)ogin, (c)reate user, (q)uit:")
    input match{
      case "L" => login()
      case "C" => createUser()
      case "Q" => logout()
      case _ => println("Invalid Input"); loginLoop()
    }
  }
  loginLoop()
}
