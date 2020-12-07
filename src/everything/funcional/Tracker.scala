package funcional

import funcional.Registo.verRegisto

import scala.annotation.tailrec
import scala.util.{Success, Try}

case class Tracker(nome:String, descricao:String, meta:Int, melhorUltrapassar:Boolean, publico:Boolean, registos:List[Registo]){
  def mudarMeta(novo:Int):Tracker = Tracker(this.nome, this.descricao, novo, this.melhorUltrapassar, this.publico, this.registos)
  def adicionarRegisto(data:String, dado:Int):(Tracker, Option[Registo]) = Tracker.adicionarRegisto(this, data,dado)
  def nAlcancados():Int = this.registos count (x => x.alcancado())
  def nTotal():Int = this.registos.length
  def percentagemAlcancado():Double = if(this.nAlcancados()>0)(this.nAlcancados()/this.nTotal())*100 else 0
  def editarRegisto(data:String, dado:Int, f:(Int,Int)=>Int):(Tracker,Option[Registo]) = Tracker.editarRegisto(this, data, dado, f)
  def encontraRegisto(data:String):Try[Registo] = Try((this.registos filter (x=>x.data.equals(data))).head)
  def atualizaRegistos(rs:List[Registo]):Tracker = Tracker.atualizaRegistos(this, rs)
  def escreve():String = Tracker.escreve(this)
  def mudaPublico(novo:Boolean):Tracker = Tracker(this.nome, this.descricao, this.meta, this.melhorUltrapassar, novo, this.registos)
  def mudarDescricao(s:String):Tracker = Tracker(this.nome, s, this.meta, this.melhorUltrapassar, this.publico, this.registos)
  //This one is not a good idea
  //def mudarNome(s:String):Tracker = Tracker(s, this.descricao, this.meta, this.melhorUltrapassar, this.publico, this.registos)
  def mudarMelhorUltrapassar(novo:Boolean):Tracker = Tracker(this.nome, this.descricao, this.meta, novo, this.publico, this.registos)
  def verRegistos():List[String] = Tracker.verRegistos(this)
  def removeRegisto(nome:String):(Tracker, Option[Registo]) = Tracker.removeRegisto(this, nome)

}

object Tracker{
  def apply(info:(String,String,Int,Boolean,Boolean)):Tracker = Tracker(info._1,info._2,info._3,info._4,info._5,List())

  //Novo
  def removeRegisto(t:Tracker, s:String):(Tracker, Option[Registo]) = {
    t.encontraRegisto(s) match {
      case Success(x) =>
        lazy val i = t.registos.indexOf(x)
        (t.atualizaRegistos(t.registos.take(i) ++ t.registos.drop(i+1)),Some(x))
      case _ => (t,None)
    }
  }

  //Alterado
  def adicionarRegisto(t:Tracker, data:String, dado:Int):(Tracker, Option[Registo]) = {
    t.registos filter (x=>x.data.equals(data)) match {
      case Nil => val novo = novoRegisto(t, data, dado)
        (t.atualizaRegistos(novo :: t.registos), Some(novo))
      case _ => (t, None)
    }
  }

  def novoRegisto(t:Tracker, data: String, dado: Int):Registo = {
    Registo(data, dado, t.meta, t.melhorUltrapassar)
  }

  //Alterado
  def editarRegisto(t:Tracker, data:String, dado:Int, f:(Int,Int)=>Int):(Tracker, Option[Registo]) = {
    t.encontraRegisto(data) match {
      case Success(x) =>
        lazy val i = t.registos.indexOf(x)
        lazy val novo = novoRegisto(t, data, f(x.dado, dado))
        (t.atualizaRegistos(t.registos.updated(i,novo)), Some(x))
      case _ => (t,None)
    }
  }

  def atualizaRegistos(t:Tracker, rs:List[Registo]):Tracker = {
    Tracker(t.nome, t.descricao,t.meta, t.melhorUltrapassar, t.publico, rs)
  }

  def escreve(t:Tracker):String = {
    lazy val inicio = t.nome + "\n" + t.descricao.replace("\n", " ") + "\n" + t.meta + "\n" + t.melhorUltrapassar + "\n" + t.publico + "\n"
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
