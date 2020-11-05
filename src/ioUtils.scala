import scala.io.StdIn.readLine

object ioUtils {
  def getUserInput(pedido: String):String = {
    print(pedido + ": ")
    val input = readLine.trim.toUpperCase
    input
  }

  def getUserInputSensitive(pedido: String):String = {
    print(pedido+ ": ")
    val input = readLine.trim
    input
  }
}
