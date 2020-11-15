package everything

import scala.util.{Failure, Success, Try}

case class User(nome:String, password:String, trackers:Map[String, Tracker]){
  def alteraTracker(t:Tracker):User = User.alteraTracker(this, t)
  def adicionaTracker(t:Tracker):User = User.adicionaTracker(this, t)
  def escreve():String = User.escreve(this)
  def verTracker():String = User.verTrackers(this)
  def existeTracker(tn:String):Try[Tracker] = Try(this.trackers(tn))
  def removeTracker(t:Tracker):User = User.removeTracker(this, t)
  def veRegistosTracker(t:Tracker):User = User.veRegistosTracker(this,t)
}

object User{
  def alteraTracker(u:User, t:Tracker): User ={
    Try(u.trackers(t.nome)) match {
      case Success(_) => User(u.nome, u.password, u.trackers + (t.nome -> t))
      case Failure(_) => println("Tracker não encontrado")
        u
    }
  }

  def adicionaTracker(u:User, t:Tracker):User = {
    Try(u.trackers(t.nome)) match {
      case Success(_) => println("Já tem um tracker com esse nome")
        u
      case Failure(_) => User(u.nome, u.password, u.trackers + (t.nome -> t))
    }
  }

  def removeTracker(u:User, t:Tracker): User ={
    Try(u.trackers(t.nome)) match {
      case Success(_) => User(u.nome, u.password, u.trackers - t.nome)
      case Failure(_) => println("Tracker não encontrado")
        u
    }
  }

  def escreve(u:User):String = {
    val inicio = u.nome + "\n" + u.password + "\nTrackers\n"
    (u.trackers.values.toList foldRight inicio) ((x,y)=>y+x.escreve())
  }

  def verTrackers(u:User):String = {
    (u.trackers map (x=> x._2.nome + ") " + x._2.descricao) foldRight "")(_+ "\n"+_)
  }

  def veRegistosTracker(u:User, t:Tracker): User = {
    t.verRegistos() foreach println
    u
  }
}