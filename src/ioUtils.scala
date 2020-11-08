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
    val file = new File("Users" + File.separator + username + ".txt")
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
    val dir = new File("Users")
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

  def writeQuiz(q:Quiz) = {
    val dir = new File("quizzes")
    dir.mkdir() //Apenas cria se nao existir essa diretoria

    val file = new File(dir.toString + File.separator + q.titulo + ".txt")
    val fos = new FileOutputStream(file, false)
    val pw = new PrintWriter(fos)

    file.createNewFile()
    pw.write(q.titulo + "\n")
    q.perguntas map (x => pw.write(x.escreve))
    pw.write("Tentativas" + q.escreveUt())
    pw.close()
  }

  def writeQuizzes(qs: List[Quiz]):List[Quiz] = qs match{
    case Nil => List()
    case h::t => writeQuiz(h); writeQuizzes(t)
  }

  def readQuizzes():List[Quiz] = {
    val files = getListOfFiles("quizzes")
    def loop(fs: List[File], qs:List[Quiz]):(List[File], List[Quiz]) = fs match {
      case Nil => (List(), qs)
      case h::t => {
        val bufferedSource = Source.fromFile(h)
        val lines = bufferedSource.getLines().toList
        bufferedSource.close
        loop(t, qs ::: List(readQuiz(lines, 0, Quiz("",List(), Map()))._3))
      }
    }
    loop(files, List())._2
  }

  def readQuiz(str:List[String], n:Int, q:Quiz):(List[String], Int, Quiz) = n match {
      //reads the title and moves to the 1st question
    case 0 => readQuiz(str.tail.tail, 1, Quiz(str(0),q.perguntas,q.ut))
      //reads a question
    case 1 => {
      val texto = str.head
      def loop(s:List[String],os:List[String]):(List[String],List[String]) = s.head match {
        case "Opcoes" => loop(s.tail, os)
        case "Correcta" => (s.tail,os)
        case x => loop(s.tail, os ::: List(x))
      }
      val opcoes = loop(str.tail, List())
      val pergunta = Pergunta(texto, opcoes._2, opcoes._1.head.toInt)

      val next = opcoes._1.tail
      next.head match{
        case "Pergunta" => readQuiz(next.tail, 1, Quiz(q.titulo, q.perguntas ::: List(pergunta), q.ut))
        case _ => println(next.head); readQuiz(next.tail, 2, Quiz(q.titulo, q.perguntas ::: List(pergunta), q.ut))
      }
      //if(next.head == "Pergunta") readQuiz(next.tail, 1, Quiz(q.titulo, q.perguntas ::: List(pergunta), q.ut)) else readQuiz(next.tail, 2, Quiz(q.titulo, q.perguntas ::: List(pergunta), q.ut))

    }
    case _ => {
      (str, n, q)
    }
  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
}
