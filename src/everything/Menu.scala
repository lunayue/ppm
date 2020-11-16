package everything

import everything.Sessao._
import everything.Utils._

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object Menu extends App{
  val users = readUsers()
  val ajudas = readAjudas()
  //val sugestoes = ???

  @tailrec
  def loginLoop():Any ={
    val options = List(
      ("1","Login"),
      ("2", "Criar Utilizador"),
      ("0", "Sair"))

    println("----[Bem Vindo]----")
    options.foreach(x =>println(x._1 + ") " + x._2))
    readLine().trim match {
      case "1" => login(users, readLine("Username: "), readLine("Password: ")) match {
        case Some(x) => mainLoop(x)
        case _ => println("Informacoes erradas")
          loginLoop()
      }
      case "2" => Sessao.createUser(users, readLine("Username: "), readLine("Password: "), readLine("Repetir Password: ")) match {
        case Some(x) => mainLoop(x)
        case _ => println("Informacoes erradas")
          loginLoop()
      }
      case "0" => sys.exit()
      case _ => println("Opcao inválida")
        loginLoop()
    }
  }

  @tailrec
  def mainLoop(user:User):User = {
    val options = List(
      ("1","Ver os meus Trackers"),
      ("2", "Editar Meta de Tracker"),
      ("3", "Tornar Tracker publico/privado"),
      ("4", "Editar descricao de Tracker"),
      ("5", "Editar nome de Tracker"),
      ("6", "Editar melhor ultrapassar meta"),
      ("7", "Ver Registos de Tracker"),
      ("8", "Adicionar Registo"),
      ("9", "Alterar Registo"),
      ("10", "Aumentar Registo"),
      ("11", "Diminuir Registo"),
      ("12", "Ver numero de registos"),
      ("13", "Ver percentagem de sucesso"),
      ("14", "Criar tracker"),
      ("15", "Adicionar tracker de outros"),
      ("16", "Ajuda"),
      ("17", "Sugestoes"),
      ("18", "Dar sugestao"),
      ("19", "Ver quizzes"),
      ("20", "Jogar"),
      ("0", "Logout"))

    println("----[Menu]----")
    options.foreach(x =>println(x._1 + ") " + x._2))
    readLine().trim match {
      case "1" => println(user.verTracker())
        mainLoop(user)
      case "2" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val m = makeInt(readLine("Nova Meta: ").trim)
        (t,m) match {
          case (Success(x), Success(y)) => mainLoop(user.alteraTracker(x.mudarMeta(y)))
          case _ => println("Tracker desconhecido ou meta invalida")
            mainLoop(user)
        }
      case "3" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val m = makeBoolean(readLine("Privado ou Publico?: ").trim.toLowerCase)
        (t,m) match {
          case (Success(x), Success(y)) => mainLoop(user.alteraTracker(x.mudaPublico(y)))
          case _ => println("Tracker desconhecido ou opcao invalida")
            mainLoop(user)
        }
      case "4" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val m = readLine("Nova Descricao: ").trim
        t match {
          case Success(x)  => mainLoop(user.alteraTracker(x.mudarDescricao(m)))
          case _ => println("Tracker desconhecido")
            mainLoop(user)
        }
      case "5" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val m = readLine("Novo nome: ").trim
        val o = user.existeTracker(m)
        (t,o) match {
          case (Success(x),Failure(_))  => mainLoop(user.removeTracker(x).adicionaTracker(x.mudarNome(m)))
          case _ => println("Tracker desconhecido ou nome ja utilizado")
            mainLoop(user)
        }
      case "6" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val m = makeBoolean(readLine("Melhor ultrapassar meta, sim ou não?: ").trim.toLowerCase)
        (t,m) match {
          case (Success(x), Success(y)) => mainLoop(user.alteraTracker(x.mudaPublico(y)))
          case _ => println("Tracker desconhecido ou opcao invalida")
            mainLoop(user)
        }
      case "7" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        t match {
          case Success(x) => mainLoop(user.veRegistosTracker(x))
          case _ => println("Tracker desconhecido")
            mainLoop(user)
        }
      case "8" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val data = readLine("Data: ").trim
        val dado = makeInt(readLine("Dado: ").trim)
        (t,dado) match {
          case (Success(x), Success(y)) => mainLoop(user.alteraTracker(x.adicionarRegisto(data,y)))
          case _ => println("Tracker desconhecido ou dado invalido")
            mainLoop(user)
        }
      case "9" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val data = readLine("Data: ").trim
        val dado = makeInt(readLine("Novo dado: ").trim)
        (t,dado) match {
          case (Success(x), Success(y)) => mainLoop(user.alteraTracker(x.editarRegisto(data,y, (_,b)=>b)))
          case _ => println("Tracker desconhecido ou dado invalido")
            mainLoop(user)
        }
      case "10" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val data = readLine("Data: ").trim
        val dado = makeInt(readLine("Aumentar em: ").trim)
        (t,dado) match {
          case (Success(x), Success(y)) => mainLoop(user.alteraTracker(x.editarRegisto(data,y, (a,b)=>a+b)))
          case _ => println("Tracker desconhecido ou dado invalido")
            mainLoop(user)
        }
      case "11" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val data = readLine("Data: ").trim
        val dado = makeInt(readLine("Diminuir em: ").trim)
        (t,dado) match {
          case (Success(x), Success(y)) => mainLoop(user.alteraTracker(x.editarRegisto(data,y, (a,b)=>a-b)))
          case _ => println("Tracker desconhecido ou dado invalido")
            mainLoop(user)
        }
      case "12" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        t match {
          case Success(x) => println("Tem " + x.nTotal() + " registos no tracker " + x.nome)
          case _ => println("Tracker desconhecido")
        }
        mainLoop(user)
      case "13" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        t match {
          case Success(x) => println("Atingiu a meta " + x.percentagemAlcancado() + "% das vezes no tracker " + x.nome)
          case _ => println("Tracker desconhecido")
        }
        mainLoop(user)
      case "14" =>
        val info = getCreateTrackerInputs
        mainLoop(user.adicionaTracker(Tracker(info)))
      case "15" =>
        val u = Try(users(readLine("User com o tracker: ").trim))
        u match {
          case Success(x) => {
            val t = Try(x.trackers(readLine("Nome do tracker: ").trim))
            t match {
              case Success(y) => if(y.publico) mainLoop(user.adicionaTracker(Tracker(y.nome, y.descricao, y.meta, y.melhorUltrapassar, y.publico)))
                else {
                  println("Tracker não encontrado")
                  mainLoop(user)
                }
              case _ => println("Tracker não encontrado")
                mainLoop(user)
            }
          }
          case _ => println("User nao encontrado")
            mainLoop(user)
        }
      case "16" => ajudas map (x=> println(x.mostra()))
        mainLoop(user)
      case "17" => readSugestoes() map (x=> println(x.mostra()))
        mainLoop(user)
      case "18" => mainLoop(user.daSugestao(Sugestao(user.nome, readLine("Titulo: ").trim, readLine("Texto: ").trim)))
      case "19" => user.podeJogar() foreach (x=>println(x.titulo + " de " + x.dono))
        mainLoop(user)
      case "20" => mainLoop(user.joga(readLine("Nome do dono: "), readLine("Titulo do quiz:" )))
      case "0" => logout(user)
      case _ => println("Opcao inválida")
        mainLoop(user)
    }
  }
  loginLoop()
}
