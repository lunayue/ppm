import scala.io.StdIn.readLine
import scala.io.Source
import java.io._

object ioUtils {
  def getUserInput(pedido: String):String = {
    print(pedido + ": ")  //O input do User mete \n na consola
    readLine.trim.toUpperCase
  }

  def getUserInputSensitive(pedido: String):String = {
    print(pedido + ": ")  //O input do User mete \n na consola
    val input = readLine.trim
    if(input.contains(" ")){
      println("Não é permitido a utilização de espaços")
      getUserInputSensitive(pedido)
    }
    input
  }

  def readUser(username: String, pass: String): Option[User] = {
    val file = new File("src"+ File.separator +"users" + File.separator + username + ".txt")
    if(file.exists()){
      val bufferedSource = Source.fromFile(file)
      if(bufferedSource.getLines.toList(1).compareTo(pass)==0) {
        bufferedSource.close
        return Some(User(username,pass))
      }
      bufferedSource.close
    }
    None
  }

  def writeUser(username: String,pass: String):Option[User] = {
    val dir = new File("src"+ File.separator +"users")
    dir.mkdir() //Apenas cria se nao existir essa diretoria

    val file = new File(dir.toString + File.separator + username + ".txt")
    if(!file.exists()){
      val fos = new FileOutputStream(file, true)
      val pw = new PrintWriter(fos)

      file.createNewFile()
      pw.write(username + "\n")
      pw.write(pass + "\n")
      pw.close()
      return Some(User(username,pass))
    } //Se o User não existir,cria uma pasta com seu nome e dados
    None
    //Devolve None se ja tiver um User com como tal username
  }
}
