import scala.io.StdIn.readLine

object menu extends App{

  def loginLoop(): Unit ={
    println("(l)ogin, (c)reate user, (q)uit:")
    val input = readLine.trim.toUpperCase
    println(printableInput(input))
    loginLoop()
    }

  def printableInput(input: String): String = input match {
    case "l" | "login" => "login"
    case "c" | "create user" => "create user"
    case "q" | "quit" => "quit"
    case _ => "Invalid Input"
  }

  loginLoop()
}

