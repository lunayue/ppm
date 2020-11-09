package tracker

import users.User

import scala.annotation.tailrec
import scala.io.StdIn.readLine

case class Tracker(user:String, nome:String, mapa:Map[String, Double], meta:Double, acima:Boolean, publico:Boolean){
  def addRecord(data:String, dado:Double):Tracker = Tracker.addRecord(this, data, dado)
  def readRecord():String = Tracker.readRecord(this.mapa)
  def readRecord(data:String):String = Tracker.readRecord(this.mapa, data)
  def removeRecord(data:String):Tracker = Tracker.removeRecord(this, data)
  def changeMeta(nova:Double):Tracker = Tracker(this.user, this.nome, this.mapa, nova, this.acima, this.publico)
  def changeAcima(novo:Boolean):Tracker = Tracker(this.user, this.nome, this.mapa, this.meta, novo, this.publico)
}

object Tracker{
  def addRecord(tracker:Tracker, data:String,dado:Double):Tracker = Tracker(tracker.user, tracker.nome, tracker.mapa + (data -> dado), tracker.meta, tracker.acima, tracker.publico)

  def readRecord(dados:Map[String, Double]):String = {
    @tailrec
    def loop(lst:List[String], str:String):(List[String], String) = lst match{
      case Nil => (List(), str)
      case h::t => loop(t, str + h + ") " + dados(h).toString + "\n" )
    }
    loop(dados.keys.toList, "")._2
  }

  def readRecord(dados:Map[String, Double], data:String):String = dados.keys.toList.indexOf(data) match {
    case -1 => "Nao foi encontrado nenhum registro dessa data"
    case _ => data + ") " + Some(dados(data)).get
  }

  def removeRecord(tracker:Tracker, data:String):Tracker = tracker.mapa.keys.toList.indexOf(data) match {
    case -1 => Tracker(tracker.user, tracker.nome, tracker.mapa, tracker.meta, tracker.acima, tracker.publico)
    case _ => Tracker(tracker.user, tracker.nome, tracker.mapa - data, tracker.meta, tracker.acima, tracker.publico)
  }

  def criarTracker(u:User):(User,Tracker) = {
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

    val user = if(publico) "default" else u.username

    (u, Tracker(user, nome, Map(), meta.toDouble, acima, publico))
  }

  def adicionarTracker(u:User, lst:List[Tracker]): Unit ={

  }
}

