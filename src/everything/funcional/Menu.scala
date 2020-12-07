package funcional

import funcional.Calculos._
import funcional.Sessao._
import funcional.Utils._

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object Menu extends App{
  lazy val users = readUsers()
  lazy val ajudas = readAjudas()

  @tailrec
  def loginLoop():Any ={
    val options = List(
      ("1","Login"), //Done
      ("2", "Criar Utilizador"), //Done
      ("0", "Sair")) //Done

    println("----[Bem Vindo]----")
    options.foreach(x =>println(x._1 + ") " + x._2))
    readLine().trim match {
      case "1" => login(users, readLine("Username: ").trim, readLine("Password: ").trim) match {
        case Some(x) => mainLoop(x)
        case _ => println("Informacoes erradas")
          loginLoop()
      }
      case "2" =>
        //Alterações
        val username = readLine("Username: ").trim
        val p1 = readLine("Password: ").trim
        val p2 = readLine("Repetir Password: ").trim
        if(Sessao.passIguais(p1, p2)){
          Sessao.createUser(users, username, p1) match {
            case Some(x) => mainLoop(x)
            case _ => println("Username já utilizado")
              loginLoop()
          }
        }
        else{
          println("As passwords devem ser iguais")
        }
        //fim de alterações

      case "0" => sys.exit()
      case _ => println("Opcao inválida")
        loginLoop()
    }
  }

  @tailrec
  def mainLoop(user:User):User = {
    val options = List(
      ("1","Ver os meus Trackers"), //Done
      ("2", "Editar Meta de Tracker"), //Done
      ("3", "Tornar Tracker publico/privado"), //Done
      ("4", "Editar descricao de Tracker"), //Done
      //("5", "Editar nome de Tracker"), retirar
      ("6", "Editar melhor ultrapassar meta"), //Done
      ("7", "Ver Registos de Tracker"), //Done
      ("8", "Adicionar Registo"), //Done
      ("9", "Alterar Registo"), //Done
      ("10", "Aumentar Registo"), //Done
      ("11", "Diminuir Registo"), //Done
      ("12", "Ver numero de registos"), //Done
      ("13", "Ver percentagem de sucesso"), //Done
      ("14", "Criar tracker"), //Done
      ("15", "Adicionar tracker de outros"), //Done
      ("16", "Ajuda"), //Done
      ("17", "Sugestoes"), //Done
      ("18", "Dar sugestao"), //Done
      ("19", "Ver quizzes"),
      ("20", "Jogar"),
      ("21", "Agua ml"), //Done
      ("22", "Agua copos"), //Done
      ("23", "BMI"), //Done
      ("24", "Poupanca"), //Done
      ("25", "Ganho"), //Done
      ("26", "Total"), //Done
      ("27", "Semanas que deve demorar a perder peso"), //Done
      ("28", "Horas que dormiu"), //Done
      ("29", "Quando se deve levantar"), //Done
      ("30", "Quando se deve deitar"), //Done
      ("31", "Ver tracker publicos"), //Done
      ("32", "Remover Tracker"), //Done
      ("33", "Remover Registo"), //Done
      ("0", "Logout")) //Done

    println("----[Menu]----")
    options.foreach(x =>println(x._1 + ") " + x._2))
    readLine().trim match {
      case "1" => println(user.verTracker())
        mainLoop(user)
      case "2" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val m = makeInt(readLine("Nova Meta: ").trim)
        (t,m) match {
          case (Success(x), Success(y)) => mainLoop(user.alteraTracker(x.mudarMeta(y))._1)
          case _ => println("Tracker desconhecido ou meta invalida")
            mainLoop(user)
        }
      case "3" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val m = makeBoolean(readLine("Privado ou Publico?: ").trim.toLowerCase)
        (t,m) match {
          case (Success(x), Success(y)) => mainLoop(user.alteraTracker(x.mudaPublico(y))._1)
          case _ => println("Tracker desconhecido ou opcao invalida")
            mainLoop(user)
        }
      case "4" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val m = readLine("Nova Descricao: ").trim
        t match {
          case Success(x)  => mainLoop(user.alteraTracker(x.mudarDescricao(m))._1)
          case _ => println("Tracker desconhecido")
            mainLoop(user)
        }
      //Retirar
      /*case "5" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val m = readLine("Novo nome: ").trim
        val o = user.existeTracker(m)
        (t,o) match {
          case (Success(x),Failure(_))  => mainLoop(user.removeTracker(x).adicionaTracker(x.mudarNome(m))._1) //Alterado, n testado
          case _ => println("Tracker desconhecido ou nome ja utilizado")
            mainLoop(user)
        }*/
      case "6" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val m = makeBoolean(readLine("Melhor ultrapassar meta, sim ou não?: ").trim.toLowerCase)
        (t,m) match {
          case (Success(x), Success(y)) => mainLoop(user.alteraTracker(x.mudaPublico(y))._1)
          case _ => println("Tracker desconhecido ou opcao invalida")
            mainLoop(user)
        }
      case "7" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        t match {
          case Success(x) =>
            x.verRegistos() map println
            mainLoop(user)
          case _ => println("Tracker desconhecido")
            mainLoop(user)
        }
      case "8" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val data = readLine("Data: ").trim
        val dado = makeInt(readLine("Dado: ").trim)
        (t,dado) match {
          case (Success(x), Success(y)) => x.adicionarRegisto(data, y) match {
            case (nt, Some(_)) =>  mainLoop(user.alteraTracker(nt)._1)
            case (_, None) => println("Nome já utilizado, utlize editar para mudar o valor")
              mainLoop(user)
          }
          case _ => println("Tracker desconhecido ou dado invalido")
            mainLoop(user)
        }
      case "9" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val data = readLine("Data: ").trim
        val dado = makeInt(readLine("Novo dado: ").trim)
        (t,dado) match {
          case (Success(x), Success(y)) => x.editarRegisto(data, y,(_,b)=>b) match {
            case (a, Some(_)) => mainLoop(user.alteraTracker(a)._1)
            case (_, None) =>
              println("Registo não encontrado")
              mainLoop(user)
          }
          case _ => println("Tracker desconhecido ou dado invalido")
            mainLoop(user)
        }
      case "10" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val data = readLine("Data: ").trim
        val dado = makeInt(readLine("Aumentar em: ").trim)
        (t,dado) match {
          case (Success(x), Success(y)) => x.editarRegisto(data, y,(a,b)=>a+b) match {
            case (a, Some(_)) => mainLoop(user.alteraTracker(a)._1)
            case (_, None) =>
              println("Registo não encontrado")
              mainLoop(user)
          }
          case _ => println("Tracker desconhecido ou dado invalido")
            mainLoop(user)
        }
      case "11" =>
        val t = user.existeTracker(readLine("Tracker: ").trim)
        val data = readLine("Data: ").trim
        val dado = makeInt(readLine("Diminuir em: ").trim)
        (t,dado) match {
          case (Success(x), Success(y)) =>  x.editarRegisto(data, y,(a,b)=>a-b) match {
            case (a, Some(_)) => mainLoop(user.alteraTracker(a)._1)
            case (_, None) =>
              println("Registo não encontrado")
              mainLoop(user)
          }
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
      //Alterado
      case "14" =>
        val info = getCreateTrackerInputs
        user.adicionaTracker(Tracker(info)) match {
          case (x,Some(_)) => println("Já tem um tracker com esse nome")
            mainLoop(x)
          case (x, None) => mainLoop(x)
        }
      //Fim de alteraçoes
      case "15" =>
        val u = Try(users(readLine("User com o tracker: ").trim))
        u match {
          case Success(x) => {
            val t = Try(x.trackers(readLine("Nome do tracker: ").trim))
            t match {
              case Success(y) => if(y.publico) mainLoop(user.adicionaTracker(Tracker(y.nome, y.descricao, y.meta, y.melhorUltrapassar, y.publico))._1) //Alterado, ñ testado
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
      case "19" => user.podeJogar() foreach (x=>println(x.titulo + " de " + x.dono + ") " + x.descricao))
        mainLoop(user)
      case "20" =>
        Try(user.quizzes((readLine("Nome do dono: "), readLine("Titulo do quiz:" )))) match {
          case Success(x) => if(user.podeJogar(x)){
            @tailrec
            def loop(lst:List[Pergunta], rs:List[Boolean]):(List[Pergunta], List[Boolean]) = lst match{
              case Nil => (List(),rs)
              case a::t => println(a.mostra)
                makeInt(readLine("Qual o nº da opcao correta? ")) match {
                  case Success(x) => a.opcaoValida(x-1) match {
                    case Success(_) => {
                      lazy val aux2 = a.correta(x-1)
                      if(aux2) println("Certo!") else println("Errado, a resposta correta era: " + a.opcoes(a.certa))
                      loop(t, aux2::rs)
                    }
                  }
                  case _ => println("Opcao invalida")
                    loop(a::t, rs)
                }
            }
            mainLoop(user.joga(x.joga(user.nome, loop(x.perguntas, List())._2)))
          }
          else {println("Não pode jogar este quiz")
            mainLoop(user)}
          case _ => println("Quiz não encontrado")
            mainLoop(user)
        }
      case "21" => {
        val idade = makeInt(readLine("Idade: "))
        val peso = makeDouble(readLine("Peso em kg: "))
        val exercicio = makeInt("Exercicio por dia, escreva em partes da hora (ex:30mins = 0.5horas): ")
        (idade, peso, exercicio) match {
          case (Success(x),Success(y),Success(z)) => println(aguaMl(x,y,z))
          case _ => println("Inputs invalidos")
        }
        mainLoop(user)
      }

      case "22" =>{
        val agua = makeInt(readLine("Agua que pretender beber em ml: "))
        val copo = makeInt(readLine("Tamanho do copo em ml: "))
        (agua, copo) match {
          case (Success(x),Success(y)) => println(aguaCopos(x,y))
          case _ => println("Inputs invalidos")
        }
        mainLoop(user)
      }
      case "23" =>{
        val m = makeDouble(readLine("Indique o ser peso em kg: "))
        val a = makeDouble(readLine("Indique a sua altura em metros: "))
        (m, a) match {
          case (Success(x),Success(y)) => println(bmi(x,y))
          case _ => println("Inputs invalidos")
        }
        mainLoop(user)
      }
      case "24" =>{
        user.existeTracker(readLine("Nome do traker: "))  match {
          case Success(x) => println(poupanca(x))
          case _ => println("Tracker nao encontrado")
        }
        mainLoop(user)
      }
      case "25" =>{
        user.existeTracker(readLine("Nome do traker: "))  match {
          case Success(x) => println(ganho(x))
          case _ => println("Tracker nao encontrado")
        }
        mainLoop(user)
      }
      case "26" =>{
        user.existeTracker(readLine("Nome do traker: "))  match {
          case Success(x) => println(total(x))
          case _ => println("Tracker nao encontrado")
        }
        mainLoop(user)
      }
      case "27" =>{
        val m1 = makeDouble(readLine("Indique o seu peso atual em kg: "))
        val m2 = makeDouble(readLine("Indique o peso que pretende alcancar em kg: "))
        (m1, m2) match {
          case (Success(x),Success(y)) => println(semanaPeso(x,y))
          case _ => println("Inputs invalidos")
        }
        mainLoop(user)
      }
      case "28" =>{
        val deita = Try(Hora(readLine("Indique apenas a hora em que se deitou: ").toInt,readLine("Indique apenas os minutos a que se deitou: ").toInt))
        val levanta = Try(Hora(readLine("Indique apenas a hora em que se levantou: ").toInt,readLine("Indique apenas os minutos a que se levantou: ").toInt))
        (deita, levanta) match {
          case (Success(x),Success(y)) => println(horasDormidas(x,y))
          case _ => println("Inputs invalidos")
        }
        mainLoop(user)
      }
      case "29" =>{
        val deita = Try(Hora(readLine("Indique apenas a hora em que se deitou: ").toInt,readLine("Indique apenas os minutos a que se deitou: ").toInt))
        val dormiu = Try(Hora(readLine("Indique apenas as horas que dormiu: ").toInt,readLine("Indique apenas os minutos que dormiu: ").toInt))
        (deita, dormiu) match {
          case (Success(x),Success(y)) => println(horasAcordar(x,y))
          case _ => println("Inputs invalidos")
        }
        mainLoop(user)
      }
      case "30" =>{
        val dormiu = Try(Hora(readLine("Indique apenas as horas que dormiu: ").toInt,readLine("Indique apenas os minutos que dormiu: ").toInt))
        val levanta = Try(Hora(readLine("Indique apenas a hora em que se levantou: ").toInt,readLine("Indique apenas os minutos a que se levantou: ").toInt))
        (dormiu, levanta) match {
          case (Success(x),Success(y)) => println(horasDeitar(x,y))
          case _ => println("Inputs invalidos")
        }
        mainLoop(user)
      }
      case "31" =>{
        user.podeAdicionarTracker(users.values.toList) map (x=>println("Tracker " + x._2.nome + " de " + x._1 + ") " + x._2.descricao))
        mainLoop(user)
      }
      case "32" =>{
        val t = user.existeTracker(readLine("Nome do tracker: ").trim)
        t match {
          case Success(y) => mainLoop(user.removeTracker(y)._1)
          case _ => println("Tracker não encontrado")
            mainLoop(user)
        }
      }
      case "33" =>{
        val t = user.existeTracker(readLine("Nome do tracker: ").trim)
        t match {
          case Success(y) => y.removeRegisto(readLine("Nome do registo: ").trim) match{
            case (z, Some(_)) => mainLoop(user.alteraTracker(z)._1)
            case (_, None) => println("Registo não encontrado")
              mainLoop(user)
          }
          case _ => println("Tracker não encontrado")
            mainLoop(user)
        }
      }
      case "0" => logout(user) //Done
      case _ => println("Opcao inválida")
        mainLoop(user)
    }
  }
  loginLoop()
}
