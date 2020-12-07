package interface

import funcional.{Pergunta, Quiz, User}
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, RadioButton, Separator, ToggleGroup}
import javafx.scene.layout.VBox
import javafx.scene.text.Text

import scala.annotation.tailrec
import scala.util.{Success, Try}

class JogaQuiz extends PassaUserQuiz {
  @FXML
  private var username : Button = _
  @FXML
  private var nome : Label =_

  @FXML
  private var mainbox : VBox = _

  var quiz:Quiz = _

  def initData(u:User, q:Quiz):Unit = {
    user = u
    quiz = q
    username.setText(user.nome)
    nome.setText(quiz.titulo + " de " + quiz.dono)
    mainbox.getChildren.add(new Label(quiz.descricao))
    mainbox.getChildren.add(new Separator())

    @tailrec
    def loop(p:List[Pergunta], g:List[ToggleGroup]):(List[Pergunta],List[ToggleGroup]) = p match {
      case Nil => (List(),g)
      case a::tail =>{
        mainbox.getChildren.add(new Text(a.texto))
        val group = new ToggleGroup();
        a.opcoes map (y=>{
          val rb = new RadioButton(y)
          rb.setToggleGroup(group)
          mainbox.getChildren.add(rb)
        })
        mainbox.getChildren.add(new Separator())
        loop(tail, group::g)
      }
    }
    val groups = loop(quiz.perguntas, List())._2

    val respostasporresponder = new Label("Tem de responder a todas as perguntas")
    respostasporresponder.setStyle("-fx-text-color: red")
    respostasporresponder.setVisible(false)
    mainbox.getChildren.add(respostasporresponder)

    val button = new Button("Enviar")
    button.setOnAction(new EventHandler[ActionEvent]() {
      override def handle(e: ActionEvent): Unit = {
        @tailrec
        def respostas(g:List[ToggleGroup], b:List[Boolean]):Option[(List[ToggleGroup],List[Boolean])] = g match {
          case Nil => Some((List(), b))
          case a::tail =>
            lazy val checkp = quiz.perguntas(quiz.perguntas.length-1-b.length)
            lazy val toggle = Try(a.getSelectedToggle.toString)
            toggle match {
              case Success(x) => lazy val idk = x.drop(x.indexOf("]")+1).replace("'", "")
                respostas(tail, (checkp.certa == checkp.opcoes.indexOf(idk))::b)
              case _ => None
            }

        }
        respostas(groups, List()) match {
          case Some(x) => val novoquiz = quiz.joga(user.nome, x._2)
            val novouser = user.joga(novoquiz)
            Utils.novaScenaResultadoQuiz(button.getScene, novouser, novoquiz, x._2)
          case _ => respostasporresponder.setVisible(true)
        }
      }
    })
    mainbox.getChildren.add(button)

  }
}
