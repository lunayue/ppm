package funcional

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class User(nome:String, password:String, trackers:Map[String, Tracker], sugestoes:List[Sugestao], quizzes: Map[(String,String),Quiz]){
  def alteraTracker(t:Tracker):(User,Option[Tracker]) = User.alteraTracker(this, t)
  def adicionaTracker(t:Tracker):(User, Option[Tracker]) = User.adicionaTracker(this, t)
  def escreve():String = User.escreve(this)
  def verTracker():String = User.verTrackers(this)
  def existeTracker(tn:String):Try[Tracker] = Try(this.trackers(tn))
  def removeTracker(t:Tracker):(User, Option[Tracker]) = User.removeTracker(this, t)
  def daSugestao(s:Sugestao):User = User.daSugestao(this, s)
  def podeJogar():List[Quiz] = User.podeJogar(this)
  def podeJogar(q:Quiz):Boolean = this.podeJogar().indexOf(q)!= -1
  def joga(q:Quiz):User = User.joga(this,q)
  def podeAdicionarTracker(us:List[User]):List[(String, Tracker)] = User.podeAdicionarTracker(this, us)
}

object User{
  //Alterado
  def alteraTracker(u:User, t:Tracker): (User, Option[Tracker]) ={
    Try(u.trackers(t.nome)) match {
      case Success(x) => (User(u.nome, u.password, u.trackers + (t.nome -> t), u.sugestoes, u.quizzes), Some(x))
      case Failure(_) => (u, None)
    }
  }

  //Alterado
  def adicionaTracker(u:User, t:Tracker):(User, Option[Tracker]) = {
    Try(u.trackers(t.nome)) match {
      case Success(x) => (u, Some(x))
      case Failure(_) => (User(u.nome, u.password, u.trackers + (t.nome -> t), u.sugestoes, u.quizzes), None)
    }
  }

  //Alterado
  def removeTracker(u:User, t:Tracker): (User, Option[Tracker]) ={
    Try(u.trackers(t.nome)) match {
      case Success(x) => (User(u.nome, u.password, u.trackers - t.nome, u.sugestoes, u.quizzes), Some(x))
      case Failure(_) => (u, None)
    }
  }

  def escreve(u:User):String = {
    lazy val inicio = u.nome + "\n" + u.password + "\nTrackers\n"
    (u.trackers.values.toList foldRight inicio) ((x,y)=>y+x.escreve())
  }

  def verTrackers(u:User):String = {
    (u.trackers map (x=> x._2.nome + ") " + x._2.descricao) foldRight "")(_+ "\n"+_)
  }

  def daSugestao(u:User, s:Sugestao):User = {
    User(u.nome, u.password, u.trackers, s::u.sugestoes, u.quizzes)
  }

  def podeAdicionarTracker(u:User, us:List[User]):List[(String, Tracker)] = {
    @tailrec
    def loop(lst:List[User], res:List[(String, Tracker)]):(List[User],List[(String, Tracker)]) = lst match {
      case Nil => (List(), res)
      case h::t => if(!h.nome.equals(u.nome)) {
        val aux = h.trackers.values.toList filter (x => x.publico && u.trackers.keys.toList.indexOf(x.nome) == -1) map (x=>(h.nome, x))
        loop(t, res ++ aux)
      } else loop(t, res)
    }
    loop(us, List())._2
  }

  def podeJogar(u:User):List[Quiz] = {
    u.quizzes.values.toList filter (x=>x.dono.equals(u.nome) || x.publico)
  }

  def joga(u:User, q:Quiz):User = User(u.nome, u.password, u.trackers, u.sugestoes, u.quizzes + ((q.dono,q.titulo) -> q) )
}