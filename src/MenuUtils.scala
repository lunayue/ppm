import scala.io.StdIn.readLine

object MenuUtils {
  def optionPrompt(str:String, options : Map[String, MenuOption[_]]) :
  Option[MenuOption[_]] = {
    println()
    println("----[" + str + "]----")
    options.keys.toList.sorted.foreach(o => println(o + ") " + options(o).nome))
    options.get(readLine().trim)
  }
}