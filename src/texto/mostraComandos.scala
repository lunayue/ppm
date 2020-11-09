package texto

import scala.annotation.tailrec

object mostraComandos extends App{
  def soma1(x:Int) = x+1

  val options = Map[String, LinhaComando[Int]](
    "1" -> new LinhaComando[Int]("Somar 1", soma1),
    "0" -> new LinhaComando[Int]("Exit", db => sys.exit)
  )

  @tailrec
  def mainLoop(x:Int) : Unit = mainLoop(
    LinhasComando.optionPrompt(options) match {
      case Some(opt) => println(opt.exec(x)); opt.exec(x)
      case _ => { println("Invalid option"); x }
    }
  )
  mainLoop(1)
}