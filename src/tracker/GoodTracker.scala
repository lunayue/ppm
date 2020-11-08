package tracker

import scala.annotation.tailrec
import scala.io.StdIn.readLine

case class GoodTracker[A](name:String, mapa:Map[String,A], meta: A){
  def addRecord(data:String, dado:A):GoodTracker[A] = GoodTracker.addRecord[A](this, data, dado)
  def readRecord():String = GoodTracker.readRecord[A](this.mapa)
  def readRecord(data:String):Option[A] = GoodTracker.readRecord[A](this.mapa, data)
  //def editRecord(data:String, dado:A, f:(A,A) => A):GoodTracker[A] = GoodTracker.editRecord[A](this, data, dado, f)
  def removeRecord(data:String):GoodTracker[A] = GoodTracker.removeRecord[A](this, data)
  def changeMeta(nova:A):GoodTracker[A] = GoodTracker[A](this.name, this.mapa, nova)

}

object GoodTracker{
  def addRecord[A](tracker:GoodTracker[A], data:String,dado:A): GoodTracker[A] = GoodTracker[A](tracker.name, tracker.mapa + (data -> dado), tracker.meta)

  def readRecord[A](dados:Map[String, A]):String = {
    @tailrec
    def loop(lst:List[String], str:String):(List[String], String) = lst match{
      case Nil => (List(), str)
      case h::t => loop(t, str + h + ":" + dados(h).toString + "\n" )
    }
    loop(dados.keys.toList, "")._2
  }

  def readRecord[A](dados:Map[String, A], data:String):Option[A] = dados.keys.toList.indexOf(data) match {
      case -1 => None
      case _ => Some(dados(data))
    }

  /*def editRecord[A](tracker: GoodTracker[A], data:String, dado:A, f:(A,A) => A):GoodTracker[A] = {
    tracker.mapa(data) match {
      case None => GoodTracker[A](tracker.name, tracker.mapa, tracker.meta)
      case x => GoodTracker[A](tracker.name, tracker.mapa + (data -> f(x, dado)), tracker.meta)
    }
  }*/

  def removeRecord[A](tracker:GoodTracker[A], data:String):GoodTracker[A] = tracker.mapa.keys.toList.indexOf(data) match {
    case -1 => GoodTracker[A](tracker.name, tracker.mapa, tracker.meta)
    case _ => GoodTracker[A](tracker.name, tracker.mapa - data, tracker.meta)
  }
}

object menu extends App{
  val a = GoodTracker("Agua em copos de 250ml", Map[String,Int](), 8)
  val b = a.addRecord("Ola", 5)
  val c = b.addRecord("Adeus", 20)
  println(c)
  println(c.readRecord())
  val d = c.removeRecord("no")
  val e = d.removeRecord("Ola")
  println(e.readRecord())
  val f = e.changeMeta(20)
  println(f)

  /*def loop[B](tracker: GoodTracker[B]):GoodTracker[B] = {
    val opcoes = List("Adicionar record", "Ler um record", "Ver todos os records", "Remover um record", "Editar a meta", "Sair")

    opcoes map (x => println(opcoes.indexOf(x)+1 + ") " + x))

    readLine.toInt match {
      case 1 => loop(tracker.addRecord("Ola", 25))
      case 2 => println(tracker.readRecord("Ola")); loop(tracker)
      case 3 => println(tracker.readRecord()); loop(tracker)
      case 4 => loop(tracker.removeRecord("Ola"))
      case 5 => loop(tracker.changeMeta(4))
      case 6 => tracker
      case _ => println("Input não reconhecido"); loop(tracker)
    }
  }*/
}
