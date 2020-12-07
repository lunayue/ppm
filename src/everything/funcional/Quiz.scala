package funcional

import funcional.Utils.makeInt

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Success, Try}

case class Quiz(dono:String, titulo: String, descricao:String, perguntas: List[Pergunta], percentagemCorretas:List[(String, Int, Double)], publico:Boolean){
  def escreve():String = Quiz.escreve(this)
  def joga(u:String, b:List[Boolean]):Quiz = Quiz.joga(this,u,b)
  //não são usadas
  def adicionaPergunta(p:Pergunta):Quiz = Quiz(this.dono, this.titulo, this.descricao, p::this.perguntas, this.percentagemCorretas, this.publico)
  def retiraPergunta(i:Int):Quiz = Quiz.retiraPergunta(this,i)
  def editaPergunta(antiga:Int, nova:Pergunta):Quiz = Quiz.editaPergunta(this, antiga, nova)
  def mudaPublico(novo:Boolean):Quiz = Quiz(this.dono, this.titulo, this.descricao, this.perguntas, this.percentagemCorretas, novo)
  def acrecentaPC(pc:(String,Int,Double)):Quiz = Quiz(this.dono, this.titulo, this.descricao, this.perguntas, pc::this.percentagemCorretas, this.publico)
}

object Quiz{
  def escreve(q:Quiz):String = {
    lazy val aux = (q.perguntas foldRight "" )(_.escreve +_)
    lazy val aux2 = (q.percentagemCorretas foldRight "")(escrevePercent(_) +_)
    q.dono + "\n" + q.titulo + "\n" + q.descricao + "\n" + q.publico + "\n"+ aux  + "Percentagens\n" + aux2
  }

  def escrevePercent(tentativa:(String,Int,Double)):String = {
    tentativa._1 + "\n" + tentativa._2 + "\n" + tentativa._3 + "\n"
  }

  def calcPercRight(ps:List[Pergunta], ce:List[Boolean]):Double = ((ce count(x=> x)) / ps.length.toDouble)*100


  def joga(q:Quiz, u:String, b:List[Boolean]):Quiz = {
    lazy val tentativas = (q.percentagemCorretas count (x => x._1.equals(u))) + 1
    lazy val pc = calcPercRight(q.perguntas, b)
    q.acrecentaPC((u, tentativas, pc))
  }

  //não é usada
  def editaPergunta(q:Quiz, a:Int, n:Pergunta): Quiz = {
    Quiz(q.dono, q.titulo, q.descricao, q.perguntas.updated(a,n), q.percentagemCorretas, q.publico)
  }

  //Impuro mas ñ é usada
  def retiraPergunta(q:Quiz, i:Int):Quiz = {
    Try(q.perguntas(i)) match {
      case Success(_) => Quiz(q.dono, q.titulo, q.descricao, q.perguntas.take(i) ++ q.perguntas.drop(i+1), q.percentagemCorretas, q.publico)
      case _ => println("Nao foi possivel retirar essa opcao")
        q
    }
  }

}
