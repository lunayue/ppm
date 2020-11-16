package everything

import java.io._

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object Utils{
  def makeInt(s:String):Try[Int] = Try(s.trim.toInt)
  def makeBoolean(s:String):Try[Boolean] = s match {
    case "publico" | "sim" => Success(true)
    case "privado" | "nao" => Success(false)
    case _ => Try(s.toBoolean)
  }

  def writeUser(user:User):User = {
    val dir = new File("users")
    dir.mkdir() //Apenas cria se nao existir essa diretoria
    val file = new File(dir.toString + File.separator + user.nome + ".txt")
    val fos = new FileOutputStream(file, false)
    val pw = new PrintWriter(fos)
    file.createNewFile()
    pw.write(user.escreve())
    pw.close()
    user
  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def readUsers():Map[String,User]={
    val files = getListOfFiles("users")
    val users = files map (x=>readUser(x))

    @tailrec
    def loop(lst:List[User], mapa:Map[String,User]):(List[User],Map[String,User]) = lst match {
      case Nil => (lst, mapa)
      case h::t => loop(t, mapa + (h.nome -> h))
    }
    loop(users, Map())._2
  }

  def readUser(file:File):User = {
    val bufferedSource = Source.fromFile(file)
    val lines = bufferedSource.getLines.toList
    bufferedSource.close

    val nome = lines.head
    val pass = lines(1)

    val trackers:(List[String],Map[String,Tracker]) = Try(lines(2)) match {
      case Success(x) => x match {
        case "Trackers" => readTrackers(lines.drop(3))
        case _ => (lines.drop(3),Map())
      }
      case _ => (lines,Map())
    }
    //Se tivermos mais cenas para ler podemos ir buscar as linhas por ler ao tracker._1
    User(nome,pass,trackers._2, List(), readQuizzes())
  }

  def readTrackers(strs:List[String]):(List[String],Map[String,Tracker]) = {
    @tailrec
    def loop(lst:List[String], ts:Map[String,Tracker]):(List[String],Map[String,Tracker]) = lst.indexOf("fim") match {
      case -1 => (lst, ts)
      case x => val t = readTracker(lst.take(x+1))
        loop(lst.drop(x+1), ts + (t.nome->t))
    }
    loop(strs, Map())
  }

  def readTracker(strs:List[String]):Tracker = {
    Tracker(strs.head, strs(1), strs(2).toInt, strs(3).toBoolean, strs(4).toBoolean, readRegistos(strs.drop(5).init))
  }

  def readRegistos(strs: List[String]):List[Registo] = {
    @tailrec
    def loop(lines:List[String], rs:List[Registo]):(List[String], List[Registo]) = lines match{
      case Nil => (List(), rs)
      case h1::h2::h3::h4::t => loop(t, readRegisto(List(h1,h2,h3,h4))::rs)
    }
    loop(strs, List())._2
  }

  def readRegisto(strs:List[String]):Registo = {
    Registo(strs.head, strs(1).toInt, strs(2).toInt, strs(3).toBoolean)
  }

  def getCreateTrackerInputs:(String, String, Int, Boolean, Boolean) = {
    val nome = readLine("Nome para o tracker: ").trim
    val descricao = readLine("Descricao para o tracker: ").trim

    @tailrec
    def loopMeta():Int = readLine("Meta a alcançar: ").trim.toIntOption match {
      case None => loopMeta()
      case x => x.get
    }

    val meta = loopMeta()
    @tailrec
    def loopAcima():Boolean = readLine("É preferivel:\n1) Ficar abaixo da meta\n2) Ficar acima da meta").trim match{
      case "1" => false
      case "2" => true
      case _ => loopAcima()
    }
    val acima = loopAcima()

    @tailrec
    def loopPublico():Boolean = readLine("Este tracker deve ficar disponivel para outras pessoas?\n1) Sim\n2) Nao").trim match{
      case "1" => true
      case "2" => false
      case _ => loopPublico()
    }
    val publico = loopPublico()
    (nome,descricao, meta, acima, publico)
  }

  def readAjudas():List[Ajuda]={
    val file = new File("Ajuda.txt")
    if(file.exists){
      val bufferedSource = Source.fromFile(file)
      val lines = bufferedSource.getLines.toList
      bufferedSource.close
      @tailrec
      def loop(i:List[String], f:List[Ajuda]):(List[String],List[Ajuda]) = i match{
        case Nil => (List(),f)
        case a::b::t => loop(t, Ajuda(a,b)::f)
      }
      loop(lines, List())._2
    }
    else List()
  }

  def readSugestoes():List[Sugestao]={
    val file = new File("Sugestoes.txt")
    if(file.exists){
      val bufferedSource = Source.fromFile(file)
      val lines = bufferedSource.getLines.toList
      bufferedSource.close
      @tailrec
      def loop(i:List[String], f:List[Sugestao]):(List[String],List[Sugestao]) = i match{
        case Nil => (List(),f)
        case a::b::c::t => loop(t, Sugestao(a,b,c)::f)
      }
      loop(lines, List())._2
    }
    else List()
  }

  def writeSugestao(sugestao:List[Sugestao]): Unit = {
    val file = new File("Sugestoes.txt")
    val fos = new FileOutputStream(file, true)
    val pw = new PrintWriter(fos)
    sugestao map (x=> pw.append(x.user + "\n" + x.titulo + "\n" + x.texto + "\n"))
    pw.close()
  }

  def readQuizzes():Map[(String,String),Quiz]={
    val files = getListOfFiles("quizzes")
    val quizzes = files map (x=>readQuiz(x))

    @tailrec
    def loop(lst:List[Quiz], mapa:Map[(String,String),Quiz]):(List[Quiz],Map[(String,String),Quiz]) = lst match {
      case Nil => (lst, mapa)
      case h::t => loop(t, mapa + ((h.dono, h.titulo) -> h))
    }
    loop(quizzes, Map())._2
  }

  def readQuiz(file:File):Quiz = {
    val bufferedSource = Source.fromFile(file)
    val lines = bufferedSource.getLines.toList
    bufferedSource.close

    val dono = lines.head
    val titulo = lines(1)
    val descricao = lines(2)
    val publico = lines(3).toBoolean
    val aux = lines.indexOf("Percentagens")
    val perguntas = readPerguntas(lines.slice(4, aux))
    val percentagems = readPercentagens(lines.drop(aux+1))
    val q = Quiz(dono, titulo, descricao, perguntas, percentagems, publico)
    println(q)
    q
  }

  def readPerguntas(lst:List[String]):List[Pergunta] = {
    @tailrec
    def loop(str:List[String], ps:List[Pergunta]):(List[String], List[Pergunta]) = str match {
      case Nil => (List(), ps)
      case a::t =>{
        val i = t.indexOf("Pergunta")
        if(i == -1) (List(), readPergunta(t)::ps)
        else{
          val p = readPergunta(t.take(i+1))
          loop(t.drop(i), p::ps)
        }
      }
    }
    loop(lst,List())._2
  }

  def readPergunta(lst:List[String]):Pergunta = {
    val texto = lst.head
    @tailrec
    def loop(le:List[String], r:List[String], c:Int):(List[String], List[String], Int) = le match {
      case Nil => (List(), r, c)
      case a::b::t if a.equals("Certa") => makeInt(b) match {
        case Success(x) =>
          (t, r, x)
        case Failure(_) => loop(b::t, a::r, c )
      }
      case a::t if a.equals("Opcoes") => loop(t, r, c)
      case a::t => loop(t, a::r,c)
    }
    val aux = loop(lst.tail, List(), -1)
    Pergunta(texto, aux._2.reverse, aux._3)
  }

  def readPercentagens(lst:List[String]):List[(String, Int, Double)] = {
    @tailrec
    def loop(l:List[String], r:List[(String, Int, Double)]):(List[String], List[(String, Int, Double)]) = l match {
      case Nil => (List(), r)
      case a::b::c::t => loop(t, (a,b.toInt, c.toDouble)::r)
    }
    loop(lst, List())._2
  }

  def writeQuizzes(qz:List[Quiz]) = {
    val dir = new File("quizzes")
    dir.mkdir()
    qz foreach (x=> writeQuiz(x))
  }

  def writeQuiz(q:Quiz)={
    val file = new File("quizzes" + File.separator + q.dono + "-" + q.titulo + ".txt")
    val fos = new FileOutputStream(file, false)
    val pw = new PrintWriter(fos)
    file.createNewFile()
    pw.write(q.escreve())
    pw.close()
  }
}
