package everything

import everything.Utils.makeInt

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Success, Try}

case class Quiz(dono:String, titulo: String, descricao:String, perguntas: List[Pergunta], percentagemCorretas:List[(String, Int, Double)], publico:Boolean){
  def adicionaPergunta(p:Pergunta):Quiz = Quiz(this.dono, this.titulo, this.descricao, p::this.perguntas, this.percentagemCorretas, this.publico)
  def retiraPergunta(i:Int):Quiz = Quiz.retiraPergunta(this,i)
  def editaPergunta(antiga:Int, nova:Pergunta):Quiz = Quiz.editaPergunta(this, antiga, nova)
  def mudaPublico(novo:Boolean):Quiz = Quiz(this.dono, this.titulo, this.descricao, this.perguntas, this.percentagemCorretas, novo)
  def escreve():String = Quiz.escreve(this)
  def joga(u:String):Quiz = Quiz.joga(this,u)
  def acrecentaPC(pc:(String,Int,Double)):Quiz = Quiz(this.dono, this.titulo, this.descricao, this.perguntas, pc::this.percentagemCorretas, this.publico)
}

object Quiz{
  def retiraPergunta(q:Quiz, i:Int):Quiz = {
    Try(q.perguntas(i)) match {
      case Success(_) => Quiz(q.dono, q.titulo, q.descricao, q.perguntas.take(i) ++ q.perguntas.drop(i+1), q.percentagemCorretas, q.publico)
      case _ => println("Nao foi possivel retirar essa opcao")
        q
    }
  }

  def editaPergunta(q:Quiz, a:Int, n:Pergunta): Quiz = {
    Quiz(q.dono, q.titulo, q.descricao, q.perguntas.updated(a,n), q.percentagemCorretas, q.publico)
  }

  def escreve(q:Quiz):String = {
    val aux = (q.perguntas foldRight "" )(_.escreve +_)
    val aux2 = (q.percentagemCorretas foldRight "")(escrevePercent(_) +_)
    q.dono + "\n" + q.titulo + "\n" + q.descricao + "\n" + q.publico + "\n"+ aux  + "Percentagens\n" + aux2
  }

  def escrevePercent(tentativa:(String,Int,Double)):String = {
    tentativa._1 + "\n" + tentativa._2 + "\n" + tentativa._3 + "\n"
  }

  def joga(q:Quiz, u:String):Quiz = {
    @tailrec
    def loop(lst:List[Pergunta], rs:List[Boolean]):(List[Pergunta], List[Boolean]) = lst match{
      case Nil => (List(),rs)
      case a::t => println(a.mostra)
        makeInt(readLine("Qual o nº da opcao correta? ")) match {
          case Success(x) => a.opcaoValida(x-1) match {
            case Success(_) => {
              val aux2 = a.correta(x-1)
              if(aux2) println("Certo!") else println("Errado, a resposta correta era: " + a.opcoes(x-1))
              loop(t, aux2::rs)
            }
          }
          case _ => println("Opcao invalida")
            loop(a::t, rs)
        }
    }
    val ce = loop(q.perguntas, List())
    val tentativas = (q.percentagemCorretas count (x => x._1.equals(u))) + 1
    val pc = calcPercRight(q.perguntas, ce._2)
    q.acrecentaPC((u, tentativas, pc))
  }

  def calcPercRight(ps:List[Pergunta], ce:List[Boolean]):Double = (ce count(x=> x)) / ps.length

}
