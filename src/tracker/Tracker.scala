package tracker

import users.User

import scala.annotation.tailrec
import scala.io.StdIn.readLine

case class Tracker(user:String, nome:String, mapa:Map[String, Double], meta:Double, acima:Boolean, publico:Boolean){
  def addRecord(user:User, data:String, dado:Double):(User,Tracker) = Tracker.addRecord(user,this, data, dado)
  def readRecord():String = Tracker.readRecord(this)
  def readRecord(data:String):String = Tracker.readRecord(this, data)
  def removeRecord(user:User, data:String):(User,Tracker) = Tracker.removeRecord(user, this, data)
  def changeMeta(nova:Double):Tracker = Tracker(this.user, this.nome, this.mapa, nova, this.acima, this.publico)
  def changeAcima(novo:Boolean):Tracker = Tracker(this.user, this.nome, this.mapa, this.meta, novo, this.publico)
  def adicionarTracker(user:User):User = Tracker.adicionarTracker(user,this)
  def mudarRecord(user:User,dado:(String,Double) ,f:(Double,Double) => Double):(User, Tracker) = Tracker.mudarRecord(this, user,dado, f)

}

object Tracker{
  //Requerem um Tracker especifico
  def addRecord(user:User, tracker:Tracker, data:String,dado:Double):(User, Tracker) ={
    val novo = Tracker(tracker.user, tracker.nome, tracker.mapa + (data -> dado), tracker.meta, tracker.acima, tracker.publico)
    (user.replaceTracker(tracker, novo), novo)
  }

  def alcancouMetas(t:Tracker): List[(String, Boolean)] ={
    if(t.acima)
      t.mapa.keys.toList.sorted map (x=>(x, t.mapa(x) >= t.meta))
    else
      t.mapa.keys.toList.sorted map (x=>(x, t.mapa(x) <= t.meta))
  }

  def alcancouMeta(dado:Double, meta:Double, acima:Boolean): Boolean = if(acima) dado >= meta else dado <= meta

  def alcancouMetaTexto(dado:Double, meta:Double, acima:Boolean):String = if(alcancouMeta(dado, meta, acima)) "Meta Alcançada" else "Meta nao alcançada"

  def readRecord(t:Tracker):String = {
    @tailrec
    def loop(lst:List[String], str:String):(List[String], String) = lst match{
      case Nil => (List(), str)
      case h::tail => loop(tail, str + h + ") " + t.mapa(h).toString + " " + alcancouMetaTexto(t.mapa(h), t.meta, t.acima) + "\n" )
    }
    def str = loop(t.mapa.keys.toList.sorted, "")._2
    str
  }

  def readRecord(t:Tracker, data:String):String = t.mapa.keys.toList.indexOf(data) match {
    case -1 => "Nao foi encontrado nenhum registro dessa data"
    case _ => data + ") " + Some(t.mapa(data)).get + " " + alcancouMetaTexto(t.mapa(data), t.meta, t.acima)
  }

  def removeRecord(u:User, tracker:Tracker, data:String):(User,Tracker) = tracker.mapa.keys.toList.indexOf(data) match {
    case -1 => (u,tracker)
    case _ =>
      val novo = Tracker(tracker.user, tracker.nome, tracker.mapa - data, tracker.meta, tracker.acima, tracker.publico)
      (u.replaceTracker(tracker, novo),novo)
  }

  def adicionarTracker(u:User,t:Tracker): User ={
    u.replaceTracker(t, Tracker(u.username, t.nome, Map(),t.meta, t.acima, t.publico))
  }

  def mudarRecord(t:Tracker, u:User, d:(String,Double), f:(Double,Double)=>Double): (User,Tracker) = if (t.mapa isDefinedAt d._1) addRecord(u, t, d._1,f(t.mapa(d._1),d._2) ) else (u,t)



  //Não requerem Tracker
  def criarTracker(u:User,nome:String,meta:Double, acima:Boolean, publico:Boolean):User = {
    val user = if(publico) "default" else u.username
    u.addTracker(Tracker(user, nome, Map(), meta.toDouble, acima, publico))
  }

  def getCreateTrackerInputs():(String, Double, Boolean, Boolean) = {
    val nome = readLine("Nome para o tracker: ").trim

    @tailrec
    def loopMeta():Double = readLine("Meta a alcançar: ").trim.toDoubleOption match {
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
    (nome,meta.toDouble, acima, publico)
  }

  def retirarRepetidos(u:String,lst:List[Tracker]):List[Tracker] = {
    val ativos = lst filter (x=>x.user.equals(u)) map (x=>x.nome)
    lst filter (x=> x.user.equals(u) || ativos.indexOf(x.nome)== -1)
  }
}

