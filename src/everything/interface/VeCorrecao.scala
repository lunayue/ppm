package interface

import funcional.{Pergunta, Quiz, User}
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, Separator}
import javafx.scene.layout.VBox

import scala.annotation.tailrec

class VeCorrecao extends MainPage {
  @FXML
  private var username : Button = _
  @FXML
  private var nome : Label =_

  @FXML
  private var mainbox : VBox = _

  var quiz:Quiz = _
  var respostas:List[Boolean] = _

  def initData(u:User, q:Quiz, b:List[Boolean]):Unit = {
    user = u
    quiz = q
    respostas = b
    username.setText(user.nome)
    nome.setText(quiz.titulo + " de " + quiz.dono)
    mainbox.getChildren.add(new Label(quiz.descricao))
    mainbox.getChildren.add(new Separator())

    @tailrec
    def loop(p:List[Pergunta], g:List[Boolean]):(List[Pergunta],List[Boolean]) = (p,g) match {
      case (Nil,Nil) => (List(),List())
      case (a::t1,b::t2) =>{
        mainbox.getChildren.add(new Label(a.texto))
        if(b) mainbox.getChildren.add(new Label("Acertou a pergunta"))
        else mainbox.getChildren.add(new Label("Errou a pergunta, a resposta correcta era: " + a.opcoes(a.certa)))
        mainbox.getChildren.add(new Separator())
        loop(t1, t2)
      }
    }
    loop(q.perguntas,respostas)
    mainbox.getChildren.add(new Label("Acertou " + (respostas count(x=>x)) + " de " + q.perguntas.length + " perguntas"))
  }
}

