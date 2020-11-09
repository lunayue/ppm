import ioUtils._
import session._
import users.User

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import tracker.Tracker._
import tracker.Tracker
/*object menu extends App{
  loginLoop()

  def loginLoop(): Unit ={
    val input = getUserInput("(l)ogin, (c)reate user, (q)uit:")
    val user = input match{
      case "L" => login(false) //case None => loginLoop()
      case "C" => createUser(false) //case None => loginLoop()
      case "Q" => exit()  //Unica função que permite proceder com o Usuario a None
      case _ => println("Invalid Input"); loginLoop()
    }

    user match {
      case None => println("Volte sempre")
      case _ => mainLoop("Oq q é suposto meter aqui? O nome do User?")
    }
  }

  @tailrec
  def mainLoop(u: String): Unit = {

    //criar um objecto como o prof fez para se poder fazer um loop em vez de um long String
    val input = getUserInput("1)perfil, 2)tracker, 3)qizzes, 4)compras, 5)alimentacao, 6)outros perfis, 7)logout")

    input match{
      case "1" => println("perfil"); mainLoop(u)

      case "2" => println("tracker"); mainLoop(u) //bons e maus,
      // bons: meta por periodo de tempo, periodo de tempo, o alcançado no periodo de tempo (criar, apagar, colocar/editar registo -> tentar dar deixar aumentar, diminuir ou alterar)
      // maus: media de dinheiro/tempo gasto num uso, usu médio por periodo de tempo, data e consumo??? (criar, apagar, colocar/editar, calculo de dinheiro/tempo gasto/poupado?)

      case "3" => println("qizzes"); mainLoop(u)
      // lista de perguntas, cada pergunta tem um conjunto de opções e 1 correcta
      // criar, editar, "jogar", fazer track do resultado ou longo de varias tentativas

      case "4" => println("compras"); mainLoop(u)
        // parecido com o tracker?

      case "5" => println("alimentacao"); mainLoop(u)
        // parecido com o tracker?
        // talvez fazer um gerador de refeição aleatorio???

      case "6" => println("outros perfis"); mainLoop(u)
        // ir buscar os outros ficheiros de user e apresentar os valores q fazem sentido, talvez tentar fazer algun sorting

      case "7" => println("logout"); logout();loginLoop();
        // largar o user e voltar para o loginLoop???
        // Gello: I think yes

      case _ => println("Invalid Input"); mainLoop(u)
    }
  }
  //mainLoop("ola")
}*/

object menu extends App {
  @tailrec
  def loginLoop():Unit = {
    val options = List(
      ("1","Login"),
      ("2","Criar Utilizador"),
      ("0","Sair"))

    println()
    println("----[Bem-Vindo]----")
    options.foreach(option => println(option._1 + ") " + option._2))

    readLine() match {
      case "1" => login() match {
        case None => loginLoop()
        case u => mainLoop(u.get)
      }
      case "2" => createUser() match {
        case None => loginLoop()
        case u => mainLoop(u.get)
      }
      case "0" => sys.exit
      case _ => println("Opcao invalida"); loginLoop()
    }

  }

  @tailrec
  def mainLoop(u: User):User = {
    val trackers = readTrackers(u, List())
    val options = List(
      ("1","Ver Perfil"),
      ("2","Trackers"),
      ("3","Quizzes"),
      ("4","Ver outros utilizadores"),
      ("0","Logout")
    )

    println()
    println("----[Opcoes]----")
    options.foreach(option => println(option._1 + ") " + option._2))

    readLine() match {
      case "1" => User("Ola", "Ola")
      case "2" => trackerLoop(u, trackers)._1
      case "3" => User("Ola", "Ola")
      case "4" => User("Ola", "Ola")
      case "0" => sys.exit //should have a save thing
      case _ => println("Opcao invalida"); mainLoop(u)
    }
  }

  @tailrec
  def trackerLoop(u:User, lst:List[Tracker]):(User,List[Tracker]) = {
    val options = List(
      ("1","Ver Trackers"),
      ("2","Adicionar Novo Tracker"),
      ("3","Criar Novo Tracker"),
      ("0","Regressar")
    )

    println()
    println("----[Trackers]----")
    options.foreach(option => println(option._1 + ") " + option._2))

    readLine() match {
      case "1" =>{
        lst map (x => if(x.user.equals(u.username)) println(x))
        (u,lst)
      }
      case "2" => lst map (x => if(x.user.equals("default")) println(x))
        (u,lst)
      case "3" =>
        val novo = criarTracker(u)
        println("Novo tracker criado com sucesso")
        trackerLoop(u, novo._2::lst)
      case "0" => writeTrackers(lst)
        (mainLoop(u),lst)
      case _ => println("Opcao invalida"); trackerLoop(u,lst)
    }

  }

  loginLoop()

}
