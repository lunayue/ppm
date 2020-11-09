package texto

object LinhasComando {

  def wrapOutput(wrapper : String, output : String) : Unit = {
    println(wrapper)
    print(output)
    println(wrapper)
  }

  def optionPrompt[A](options : Map[String, LinhaComando[A]]) :
  Option[LinhaComando[A]] = {
    println()
    println("----[Opcoes]----")
    options.foreach(option => println(option._1 + ") " + option._2.nome))
    options.get(prompt("Action").toLowerCase())
  }

  def prompt(msg : String) : String = {
    print(msg + ": ")
    scala.io.StdIn.readLine()
  }

}
