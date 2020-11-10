import MenuUtils.optionPrompt
import session._
import users.User

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import tracker.Tracker
import tracker.Tracker.adicionarTracker

object menu extends App {
  @tailrec
  def loginLoop():Unit = {
    val options = Map(
      "1" -> MenuOption("Login", ()=>mainLoop(login().get)),
      "2"-> MenuOption("Criar Utilizador", ()=>mainLoop(createUser().get)),
      "0" -> MenuOption("Sair", () => sys.exit))

    optionPrompt("Bem-Vindo", options) match {
      case Some(x) => x.exec()
      case _ => println("Invalid Input"); loginLoop()
    }
  }

  @tailrec
  def mainLoop(u: User):User = {
    val options = Map(
      "1" -> MenuOption[User]("Ver Perfil", ()=>{println("todo");u}),
      "2" -> MenuOption[User]("Trackers", ()=>trackerLoop(u)),
      "3" -> MenuOption[User]("Quizzes", ()=>{println("todo");u}),
      "4" -> MenuOption[User]("Ver outros utilizadores", ()=>{println("todo");u}),
      //"0" -> MenuOption("Logout", ()=>{logout(u)})
    )

    optionPrompt("Bem-Vindo", options) match {
      case Some(x) => x.exec()
        u
      case _ => println("Opcao invalida"); mainLoop(u)
    }
  }

  @tailrec
  def trackerLoop(u:User):User = {
    val options = Map(
      "1" -> MenuOption("Ver Trackers", ()=>verTrackerLoop(u)),
      "2" -> MenuOption("Adicionar Novo Tracker", ()=>verPodeAdicionarLoop(u)),
      "3" -> MenuOption("Criar Novo Tracker", ()=>println("todo")),
      "0" -> MenuOption("Regressar", ()=>mainLoop(u))
    )

    optionPrompt("Trackers", options) match {
      case Some(x) => x.exec();u;
      case _ => println("Opcao invalida"); trackerLoop(u)
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

    val options = Map(
      "1"->MenuOption[(User,Tracker)]("Ver Informação", ()=>{println(t); (u,t)}),
      "2"->MenuOption[(User,Tracker)]("Procurar Registo", ()=>{t.readRecord(readLine("Data: ").trim); (u,t)}),
      "3"->MenuOption[(User,Tracker)]("Adicionar Registo", ()=>{val res = t.addRecord(u,readLine("Data: ").trim,readLine("Dado: ").trim.toDouble); trackerEscolhidoLoop(res._1, res._2)}),
      "4"->MenuOption[(User,Tracker)]("Apagar Registo", ()=>t.removeRecord(u,readLine("Data: ").trim)),
      "0"->MenuOption[(User,Tracker)]("Regressar", ()=>(verTrackerLoop(u),t))
    )

    optionPrompt("Tracker: " + t.nome + " Meta: " + t.meta + " Registos: " + t.mapa.keys.toList.length, options) match {
      case Some(x) => x.exec();(u,t);
      case _ => println("Opcao invalida"); (trackerLoop(u),t)
    }
  }

  loginLoop()
}
