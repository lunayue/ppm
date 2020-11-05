import scala.io.StdIn.readLine

object menu extends App{

  def loginLoop(): Unit ={
    println("(l)ogin, (c)reate user, (q)uit:")
    val input = readLine.trim.toUpperCase
    input match{
      case "L" => {
        println("login")
        loginLoop()
      }
      case "C" => {
        println("create user")
      }
      case "Q" => {
        println("quit")
      }
      case _ => {
        println("Invalid Input")
      }
    }
  }

  loginLoop()
}
