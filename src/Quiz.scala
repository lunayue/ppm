import Quiz._
import ioUtils._

import scala.annotation.tailrec
import scala.io.StdIn.readLine

//Devo acabar por dividir isto em vários ficheiros e por num folder/package diferent?

//Quiz
case class Quiz(titulo: Titulo, perguntas: List[Pergunta], ut: UsersTries){
  def play():Quiz = Quiz.play(this)
  def escreveUt():String = Quiz.escreveUt(ut:UsersTries)
}

object Quiz{
  type Titulo = String
  type UsersTries = Map[String, List[(Pergunta, Int, Boolean)]]

  def play(q:Quiz):Quiz = {
    def jogadas(ps:List[Pergunta], rs: List[(Pergunta, Int, Boolean)]):(List[Pergunta], List[(Pergunta, Int, Boolean)]) = ps match {
      case Nil => (ps, rs)
      case h::t => {
        h.mostra()
        println("Escolhe uma das opcoes")
        val n = h.opcaoValida(readLine.toInt)
        loop(n)
        @tailrec
        def loop(n:Int):Int = n match {
          case -1 => loop(h.opcaoValida(readLine.toInt))
          case _ => n
        }
        jogadas(t, (h, n, h.correta(n))::rs)
      }
    }
    val j = jogadas(q.perguntas, List())

    println()

    q.ut.contains("Ola") match{
      case false => Quiz(q.titulo, q.perguntas, q.ut + ("Ola" -> j._2.reverse))
      case true => val nova = q.ut("Ola") ::: j._2.reverse; Quiz(q.titulo, q.perguntas, q.ut + ("Ola" -> nova))
    }
  }

  def escreveUt(ut:UsersTries):String = {
    def loop(k:List[String], str:String):(List[String], String) = k match{
      case Nil => (List(), str)
      case a::tail => loop(tail,escreveTentativa(a, ut(a)))
    }
    loop(ut.keys.toList,"")._2
  }

  def escreveTentativa(s: String, lst: List[(Pergunta, Int, Boolean)]):String = {
    val inicio = "\n" + s
    def loop(l: List[(Pergunta, Int, Boolean)], str:String):(List[(Pergunta, Int, Boolean)], String) = l match{
      case Nil => (List(), str)
      case a::tail => loop(tail, str + "\n" + a._1 + "\n" + a._2 + "\n" + a._3)
    }
    inicio + loop(lst, "")._2
  }

  /*def corretas(rs: List[(Pergunta,Int)]) = {
    def loop(ls: List[(Pergunta, Int)], n:Int):(List[(Pergunta, Int)], Int) = ls match {
      case Nil => (ls, n)
      case h::t => if(h._2-1 == h._1.certa) loop(t, n+1) else loop(t, n)
    }
    loop(rs, 0)
  }*/
}

//Pergunta
case class Pergunta(texto: String, opcoes: List[String], certa: Int){
  def adicionaOpcao(opcao:String): Pergunta = Pergunta.adicionaOpcao(this, opcao)
  def editarCorreta(n:Int): Pergunta = Pergunta.editarCorrecta(this, n)
  def mostra() = Pergunta.mostra(this)
  def opcaoValida(n:Int): Int = Pergunta.opcaoValida(this.opcoes, n)
  def correta(n:Int):Boolean = Pergunta.correta(this.certa, n)
  def escreve():String = Pergunta.escreve(this)
}

object Pergunta{
  def adicionaOpcao(pergunta: Pergunta, opcao: String):Pergunta = {
    Pergunta(pergunta.texto, pergunta.opcoes ++ List(opcao), pergunta.certa)
  }
  def editarCorrecta(pergunta:Pergunta, n:Int):Pergunta = Pergunta(pergunta.texto, pergunta.opcoes, n)
  def mostra(pergunta: Pergunta) = {
    println(pergunta.texto)
    pergunta.opcoes map println
  }
  def opcaoValida(opcoes:List[String], n:Int):Int = n match{
    case x if x > 0 && x <= opcoes.length => x
    case _ => -1
  }
  def correta(c:Int, n:Int):Boolean = (c == n)

  def escreve(p:Pergunta):String = {
    val inicio = "Pergunta\n" + p.texto + "\n" + "Opcoes\n"
    def loop(lst: List[String], s:String):(List[String], String) = lst match {
      case Nil => (List(), s)
      case a::tail => loop(tail, s + a + "\n")
    }
    inicio + loop(p.opcoes, "")._2 + "Correcta\n" + p.certa + "\n"
  }
}

//"Main"

object quizzes extends App{
  //val qzs = List[Quiz]()
  //quizLoop(qzs)
  quizLoop(readQuizzes())

  @tailrec
  def quizLoop(qs: List[Quiz]):List[Quiz] ={
    println("(c)reate quizz, (p)lay quiz, (e)dit quiz, (l)eave")
    val input = readLine.trim.toUpperCase
    input match{
      case "C" => quizLoop(qs ++ List(adicionarPergunta(criarQuiz())))
      case "P" => pickQuiz(qs)
      case "E" => println("edit"); qs
      case "L" => println("leave"); writeQuizzes(qs)
    }
  }

  def criarQuiz():Quiz = {
    println("Qual o titulo para o quiz")
    val input = readLine.trim
    Quiz(input, List(), Map())
  }

  @tailrec
  def adicionarPergunta(quiz:Quiz):Quiz = {
    println("Adicionar mais perguntas?")
    readLine.trim.toUpperCase match {
      case "S" | "SIM" | "Y" | "YES" =>
        println("Escreva a pergunta")
        val texto = readLine.trim
        val p1 = adicionarOpcoes(Pergunta(texto, List(), -1))
        val p2 = decidirCorreta(p1)
        adicionarPergunta(Quiz(quiz.titulo, quiz.perguntas ++ List(p2), quiz.ut))

      case "N" | "NAO" | "NO" => if(quiz.perguntas.length < 1){println("Um questionário precisa de pelo menos 1 pergunta"); adicionarPergunta(quiz)} else quiz
      case _ => println("Input não reconhecido"); adicionarPergunta(quiz)
    }
  }

  @tailrec
  def adicionarOpcoes(pergunta: Pergunta):Pergunta = {
    println("Adicionar mais opcoes?")
    readLine.trim.toUpperCase match {
      case "S" | "SIM" | "Y" | "YES" => println("Escreva a opcao"); adicionarOpcoes(pergunta.adicionaOpcao(readLine.trim))
      case "N" | "NAO" | "NO" => if(pergunta.opcoes.length < 2) {println("Uma pergunta precisa de pelo menos 2 opções"); adicionarOpcoes(pergunta)} else pergunta
      case _ => println("Input não reconhecido"); adicionarOpcoes(pergunta)
    }
  }

  @tailrec
  def decidirCorreta(pergunta: Pergunta):Pergunta = {
    println("Qual a opção correta?")
    pergunta.opcoes map (x => println(x))
    readLine.toInt match {
      case x if x > 0 && x <= pergunta.opcoes.length => pergunta.editarCorreta(x)
      case _ => println("Input invalido"); decidirCorreta(pergunta)
    }
  }

  def pickQuiz(quizzes: List[Quiz]):List[Quiz] = {
    println("Pick a quiz to play")
    quizzes map println
    readLine().toInt match{
      case x if x > 0 && x <= quizzes.length => quizLoop(quizzes.updated(x-1,quizzes(x-1).play()))
      case -1 => quizLoop(quizzes)
      case _ => println("Unknown input"); pickQuiz(quizzes)
    }
  }
}


