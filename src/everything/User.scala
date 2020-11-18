package everything

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class User(nome:String, password:String, trackers:Map[String, Tracker], sugestoes:List[Sugestao], quizzes: Map[(String,String),Quiz]){
  def alteraTracker(t:Tracker):User = User.alteraTracker(this, t)
  def adicionaTracker(t:Tracker):User = User.adicionaTracker(this, t)
  def escreve():String = User.escreve(this)
  def verTracker():String = User.verTrackers(this)
  def existeTracker(tn:String):Try[Tracker] = Try(this.trackers(tn))
  def removeTracker(t:Tracker):User = User.removeTracker(this, t)
  def veRegistosTracker(t:Tracker):User = User.veRegistosTracker(this,t)
  def daSugestao(s:Sugestao):User = User.daSugestao(this, s)
  def podeJogar():List[Quiz] = User.podeJogar(this)
  def joga(d:String, t:String):User = User.joga(this,d,t)
  def podeAdicionarTracker(us:List[User]):List[(String, Tracker)] = User.podeAdicionarTracker(this, us)
}

object User{
  def alteraTracker(u:User, t:Tracker): User ={
    Try(u.trackers(t.nome)) match {
      case Success(_) => User(u.nome, u.password, u.trackers + (t.nome -> t), u.sugestoes, u.quizzes)
      case Failure(_) => println("Tracker não encontrado")
        u
    }
  }

  def adicionaTracker(u:User, t:Tracker):User = {
    Try(u.trackers(t.nome)) match {
      case Success(_) => println("Já tem um tracker com esse nome")
        u
      case Failure(_) => User(u.nome, u.password, u.trackers + (t.nome -> t), u.sugestoes, u.quizzes)
    }
  }

  def removeTracker(u:User, t:Tracker): User ={
    Try(u.trackers(t.nome)) match {
      case Success(_) => User(u.nome, u.password, u.trackers - t.nome, u.sugestoes, u.quizzes)
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
  def daSugestao(u:User, s:Sugestao):User = {
    User(u.nome, u.password, u.trackers, s::u.sugestoes, u.quizzes)
  }

  def podeJogar(u:User):List[Quiz] = {
    u.quizzes.values.toList filter (x=>x.dono.equals(u.nome) || x.publico)
  }

  def joga(u:User, d:String, t:String):User = {
    Try(u.quizzes((d,t))) match {
      case Success(x) => if(x.publico || x.dono.equals(u.nome)){
        User(u.nome, u.password, u.trackers, u.sugestoes, u.quizzes + (((d,t) -> x.joga(u.nome)) ))
      }  else{println("Quiz privado"); u}

      case Failure(_) => println("Quiz desconhecido")
        u
    }
  }

  def podeAdicionarTracker(u:User, us:List[User]):List[(String, Tracker)] = {
    @tailrec
    def loop(lst:List[User], res:List[(String, Tracker)]):(List[User],List[(String, Tracker)]) = lst match {
      case Nil => (List(), res)
      case h::t => if(!h.nome.equals(u.nome)) {
          val aux = h.trackers.values.toList filter (x => x.publico) map (x=>(h.nome, x))
          loop(t, res ++ aux)
        } else loop(t, res)
      }
    loop(us, List())._2
  }
}