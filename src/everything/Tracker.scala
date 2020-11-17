package everything

import everything.Registo.verRegisto

import scala.annotation.tailrec
import scala.util.{Success, Try}

case class Tracker(nome:String, descricao:String, meta:Int, melhorUltrapassar:Boolean, publico:Boolean, registos:List[Registo]){
  def mudarMeta(novo:Int):Tracker = Tracker(this.nome, this.descricao, novo, this.melhorUltrapassar, this.publico, this.registos)
  def adicionarRegisto(data:String, dado:Int):Tracker = Tracker.adicionarRegisto(this, data,dado)
  def nAlcancados():Int = this.registos count (x => x.alcancado())
  def nTotal():Int = this.registos length
  def percentagemAlcancado():Double = (this.nAlcancados()/this.nTotal())*100
  def editarRegisto(data:String, dado:Int, f:(Int,Int)=>Int):Tracker = Tracker.editarRegisto(this, data, dado, f)
  def encontraRegisto(data:String):Try[Registo] = Try((this.registos filter (x=>x.data.equals(data))).head)
  def atualizaRegistos(rs:List[Registo]):Tracker = Tracker.atualizaRegistos(this, rs)
  def escreve():String = Tracker.escreve(this)
  def mudaPublico(novo:Boolean):Tracker = Tracker(this.nome, this.descricao, this.meta, this.melhorUltrapassar, novo, this.registos)
  def mudarDescricao(s:String):Tracker = Tracker(this.nome, s, this.meta, this.melhorUltrapassar, this.publico, this.registos)
  def mudarNome(s:String):Tracker = Tracker(s, this.descricao, this.meta, this.melhorUltrapassar, this.publico, this.registos)
  def mudarMelhorUltrapassar(novo:Boolean):Tracker = Tracker(this.nome, this.descricao, this.meta, novo, this.publico, this.registos)
  def verRegistos():List[String] = Tracker.verRegistos(this)

}

object Tracker{
  def apply(info:(String,String,Int,Boolean,Boolean)):Tracker = Tracker(info._1,info._2,info._3,info._4,info._5,List())
  def adicionarRegisto(t:Tracker, data:String, dado:Int):Tracker = {
    t.registos filter (x=>x.data.equals(data)) match {
      case Nil =>  t.atualizaRegistos(novoRegisto(t, data, dado) :: t.registos)
      case _ => println("Registo já existente, edite para alterar")
        t
    }
  }

  def novoRegisto(t:Tracker, data: String, dado: Int):Registo = {
    Registo(data, dado, t.meta, t.melhorUltrapassar)
  }

  def editarRegisto(t:Tracker, data:String, dado:Int, f:(Int,Int)=>Int):Tracker = {
    t.encontraRegisto(data) match {
      case Success(x) =>
        val i = t.registos.indexOf(x)
        val novo = novoRegisto(t, data, f(x.dado, dado))
        t.atualizaRegistos(t.registos.updated(i,novo))
      case _ => println("Registo não encontrado, faça adicionar registo")
        t
    }
  }

  def atualizaRegistos(t:Tracker, rs:List[Registo]):Tracker = {
    Tracker(t.nome, t.descricao,t.meta, t.melhorUltrapassar, t.publico, rs)
  }

  def escreve(t:Tracker):String = {
    val inicio = t.nome + "\n" + t.descricao + "\n" + t.meta + "\n" + t.melhorUltrapassar + "\n" + t.publico + "\n"
    @tailrec
    def loop(rs:List[Registo], str:String):(List[Registo],String) = rs match {
      case Nil => (List(),str)
      case h::tail => loop(tail, str+h.escreve())
    }
    loop(t.registos, inicio)._2 + "fim\n"
  }

  def verRegistos(t:Tracker):List[String] = {
    t.registos map (x=>verRegisto(x))
  }

}
