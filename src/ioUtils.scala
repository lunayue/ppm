import scala.io.StdIn.readLine
import scala.io.Source
import java.io._

import users.User
import tracker.Tracker
import tracker.Tracker._

import scala.annotation.tailrec

object ioUtils {
  //Ñ tenho tado a usar
  def getUserInput(pedido: String):String = {
    print(pedido + ": ")  //O input do User mete \n na consola
    readLine.trim.toUpperCase
  }

  //Ñ tenho tado a usar
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
        return Some(User(username,pass, readTrackers(username)))
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
      return Some(User(username,pass, List()))
    } //Se o User não existir,cria uma pasta com seu nome e dados
    None
    //Devolve None se ja tiver um User com como tal username
  }

  def writeQuiz(q:Quiz): Unit = {
    val dir = new File("quizzes")
    dir.mkdir() //Apenas cria se nao existir essa diretoria

    val file = new File(dir.toString + File.separator + q.titulo + ".txt")
    val fos = new FileOutputStream(file, false)
    val pw = new PrintWriter(fos)

    file.createNewFile()
    pw.write(q.titulo + "\n")
    q.perguntas foreach (x => pw.write(x.escreve))
    pw.write("Tentativas" + q.escreveUt())
    pw.close()
  }

  @tailrec
  def writeQuizzes(qs: List[Quiz]):List[Quiz] = qs match{
    case Nil => List()
    case h::t => writeQuiz(h); writeQuizzes(t)
  }

  def readQuizzes():List[Quiz] = {
    val files = getListOfFiles("quizzes")
    @tailrec
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

  @tailrec
  def readQuiz(str:List[String], n:Int, q:Quiz):(List[String], Int, Quiz) = n match {
      //reads the title and moves to the 1st question
    case 0 => readQuiz(str.tail.tail, 1, Quiz(str.head,q.perguntas,q.ut))
      //reads a question
    case 1 => {
      val texto = str.head
      @tailrec
      def loop(s:List[String], os:List[String]):(List[String],List[String]) = s.head match {
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
    case _ => (str, n, q)
  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def writeTracker(t:Tracker){
    val dir = new File("trackers")
    dir.mkdir()

    val file = new File(dir.toString + File.separator + t.user + "-" + t.nome + ".txt")

    val fos = new FileOutputStream(file, false)
    val pw = new PrintWriter(fos)

    file.createNewFile()
    pw.write(t.user + "\n")
    pw.write(t.nome + "\n")
    pw.write(t.meta + "\n")
    pw.write(t.publico + "\n")
    pw.write(t.acima + "\n")

    t.mapa.foreach (x => pw.write(x._1 + "\n" + x._2 + "\n"))
    println("ola")
    pw.close()
  }

  @tailrec
  def writeTrackers(ts:List[Tracker]):List[Tracker] = ts match{
    case Nil => List()
    case a::t =>
      writeTracker(a)
      writeTrackers(t)
  }


  def readTrackers(u:String): List[Tracker] ={
    val files = getListOfFiles("trackers")

    @tailrec
    def loopFiles(fs:List[File], ts:List[Tracker]):(List[File],List[Tracker]) = fs match {
      case Nil => (List(),ts)
      case a::t => readTracker(a, u) match {
        case None => loopFiles(t,ts)
        case x => loopFiles(t, x.get::ts)
      }
    }
    retirarRepetidos(u,loopFiles(files, List())._2)
  }

  def readTracker(file:File, u:String):Option[Tracker] = {
    if(file.exists()){
      val bufferedSource = Source.fromFile(file)
      val lines = bufferedSource.getLines.toList
      val username = lines.head
      if(username.equals(u) || username.equals("default")){
        val tracker = Tracker(username, lines(1), Map(), lines(2).toDouble, lines(3).toBoolean, lines(4).toBoolean )
        if(username.equals("default")) Some(tracker)
        else{
          val mapaLines = lines.drop(5)
          def loopMapa(ls:List[String], mp:Map[String,Double]):(List[String], Map[String,Double]) = ls match {
            case Nil => (List(), mp)
            case a::b::t => (t, mp + (a->b.toDouble))
          }
          Some(Tracker(tracker.user, tracker.nome, loopMapa(mapaLines,Map())._2, tracker.meta, tracker.acima, tracker.publico))
        }
      }
      else{
        bufferedSource.close
        None
      }
    }
    else None
  }

}
