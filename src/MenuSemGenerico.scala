import session.{createUser, login, logout}
import tracker.Tracker
import tracker.Tracker.adicionarTracker
import users.User

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object MenuSemGenerico extends App{
  @tailrec
  def loginLoop():Unit = {
    val options = List(
      ("1","Login"),
      ("2", "Criar Utilizador"),
      ("0", "Sair"))

    options.foreach(x =>println(x._1 + ") " + x._2))

    println("----[Bem Vindo]----")
    readLine().trim match {
      case "1" => mainLoop(login().get)
      case "2" => mainLoop(createUser().get)
      case "0" => sys.exit()
      case _ => println("Opcao inválida")
        loginLoop()
    }
  }

  @tailrec
  def mainLoop(u: User):User = {
    val options = List(
      ("1", "Ver Perfil"),
      ("2", "Trackers"),
      ("3", "Quizzes"),
      ("4", "Ver outros utilizadores"),
      ("0", "Logout")
    )

    options.foreach(x =>println(x._1 + ") " + x._2))

    println("----[Opcoes]----")
    readLine().trim match {
      case "1" => println("todo")
        mainLoop(u)
      case "2" => trackerLoop(u)
      case "3" => println("todo")
        mainLoop(u)
      case "4" => println("todo")
        mainLoop(u)
      case "0" => logout(u)
        u
      case _ => println("Opcao inválida")
        mainLoop(u)
    }
  }

  @tailrec
  def trackerLoop(u:User):User = {
    val options = List(
      ("1","Ver Trackers"),
      ("2","Adicionar Novo Tracker"),
      ("3","Criar Novo Tracker"),
      ("0","Regressar")
    )

    options.foreach(x =>println(x._1 + ") " + x._2))

    println("----[Trackers]----")
    readLine().trim match {
      case "1" => verTrackerLoop(u)
      case "2" => verPodeAdicionarLoop(u)
      case "3" => println("todo")
        trackerLoop(u)
      case "0" => mainLoop(u)
      case _ => println("Opcao inválida")
        trackerLoop(u)
    }
  }

  @tailrec
  def verTrackerLoop(u: User):User = {
    println("----[Os Seus Trackers]----")
    val ativos = u.trackers filter (x => x.user.equals(u.username))
    ativos foreach (x => println(ativos.indexOf(x) + 1 + ") " + x.nome))
    println("0) Regressar")
    readLine().trim match {
      case "0" => trackerLoop(u)
      case x if x.toInt <= ativos.length =>
        trackerEscolhidoLoop(u, ativos(x.toInt - 1))._1
      case _ => println("Input não válido")
        verTrackerLoop(u)
    }
  }

  @tailrec
  def verPodeAdicionarLoop(u:User):User = {
    println("----[Disponiveis]----")
    val disponiveis = u.trackers filter (x => x.user.equals("default"))
    disponiveis foreach (x => println(disponiveis.indexOf(x) + 1 + ") " + x.nome))
    println("0) Regressar")
    readLine().trim match {
      case "0" => trackerLoop(u)
      case x if x.toInt <= disponiveis.length =>
        trackerLoop(adicionarTracker(u, disponiveis(x.toInt - 1)))
      case _ => println("Input não válido")
        verPodeAdicionarLoop(u)
    }
  }

  def trackerEscolhidoLoop(u:User, t:Tracker):(User,Tracker) = {

    val options = List(
      ("1","Ver Informação"),
      ("2","Procurar Registo"),
      ("3","Adicionar Registo"),
      ("4","Apagar Registo"),
      ("0","Regressar")
    )

    println("----[Tracker " + t.nome + "]----")
    println("Meta: " + t.meta + " Registos: " + t.mapa.keys.toList.length)
    options.foreach(x =>println(x._1 + ") " + x._2))
    readLine().trim match {
      case "1" => println("todo")
        trackerEscolhidoLoop(u,t)
      case "2" => t.readRecord(readLine("Data: ").trim)
        trackerEscolhidoLoop(u,t)
      case "3" => val res = t.addRecord(u,readLine("Data: ").trim,readLine("Dado: ").trim.toDouble)//ñ ta completo
        trackerEscolhidoLoop(res._1, res._2)
      case "4" => val res = t.removeRecord(u,readLine("Data: ").trim)
        trackerEscolhidoLoop(res._1, res._2)
        trackerEscolhidoLoop(u,t)
      case "0" => (verTrackerLoop(u),t)
      case _ => println("Opcao inválida")
        trackerEscolhidoLoop(u,t)
    }
  }
  loginLoop()
}