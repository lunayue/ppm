package interface

import funcional.Utils.{makeDouble, makeInt}
import funcional.{Calculos, Hora, User}
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextField}

import scala.util.{Success, Try}

class CalculosAuxiliares extends MainPage {
  @FXML
  private var acordarvazio: Label = _
  @FXML
  private var acordarinvalido: Label = _
  @FXML
  private var deitarvazio: Label = _
  @FXML
  private var deitarinvalido: Label = _
  @FXML
  private var dormiuvazio: Label = _
  @FXML
  private var dormiuinvalido: Label = _
  @FXML
  private var pesovazio: Label = _
  @FXML
  private var pesoinvalido: Label = _
  @FXML
  private var imcvazio: Label = _
  @FXML
  private var imcinvalido: Label = _
  @FXML
  private var copovazio: Label = _
  @FXML
  private var copoinvalido: Label = _
  @FXML
  private var aguavazio: Label = _
  @FXML
  private var aguainvalido: Label = _

  @FXML
  private var acordarresultado: TextField = _
  @FXML
  private var acordardeitahora: TextField = _
  @FXML
  private var acordardurmiuhora: TextField = _
  @FXML
  private var acordardeitaminuto: TextField = _
  @FXML
  private var acordardurmiuminuto: TextField = _
  @FXML
  private var deitarresultado: TextField = _
  @FXML
  private var deitardurmiuminuto: TextField = _
  @FXML
  private var deitaracordaminuto: TextField = _
  @FXML
  private var deitardurmiuhora: TextField = _
  @FXML
  private var deitaracordahora: TextField = _
  @FXML
  private var dormiuresultado: TextField = _
  @FXML
  private var dormiuacordaminuto: TextField = _
  @FXML
  private var dormiudeitaminuto: TextField = _
  @FXML
  private var dormiuacordahora: TextField = _
  @FXML
  private var dormiudeitahora: TextField = _
  @FXML
  private var aguaidade: TextField = _
  @FXML
  private var aguapeso: TextField = _
  @FXML
  private var aguaexercicio: TextField = _
  @FXML
  private var aguaresultado: TextField = _
  @FXML
  private var copoagua: TextField = _
  @FXML
  private var copocapacidade: TextField = _
  @FXML
  private var coporesultado: TextField = _
  @FXML
  private var imcpeso: TextField = _
  @FXML
  private var imcresultado: TextField = _
  @FXML
  private var imcaltura: TextField = _
  @FXML
  private var pesoatual: TextField = _
  @FXML
  private var pesonovo: TextField = _
  @FXML
  private var pesoresultado: TextField = _

  @FXML
  private var username : Button = _

  override def initData(u:User):Unit = {
    user = u
    username.setText(user.nome)
  }

  def validaInputHora(s1:String, s2:String):Try[Hora] = Try(Hora(s1.toInt, s2.toInt))

  def aguapordia():Unit = {
    (aguaidade.getText, aguapeso.getText, aguaexercicio.getText) match {
      case (a, b, c) if a.length == 0 || b.length == 0 || c.length == 0 =>
        aguavazio.setVisible(true)
        aguainvalido.setVisible(false)
      case (a,b,c) =>
        aguavazio.setVisible(false)
        (makeInt(a), makeDouble(b), makeDouble(c)) match {
          case (Success(x), Success(y), Success(z)) if x > 0 && y > 0 && z >= 0 =>
            aguainvalido.setVisible(false)
            aguaresultado.setText(Calculos.aguaMl(x,y,z).toString)
          case _ =>
            aguainvalido.setVisible(true)
        }
    }
  }
  def coposagua():Unit = {
    (copoagua.getText, copocapacidade.getText) match {
      case (a, b) if a.length == 0 || b.length == 0 =>
        copovazio.setVisible(true)
        copoinvalido.setVisible(false)
      case (a,b) =>
        copovazio.setVisible(false)
        (makeInt(a), makeInt(b)) match {
          case (Success(x), Success(y)) if x > 0 && y > 0 =>
            copoinvalido.setVisible(false)
            coporesultado.setText(Calculos.aguaCopos(x,y).toString)
          case _ =>
            copoinvalido.setVisible(true)
        }
    }
  }

  def imc():Unit ={
    (imcpeso.getText, imcaltura.getText) match {
      case (a, b) if a.length == 0 || b.length == 0 =>
        imcvazio.setVisible(true)
        imcinvalido.setVisible(false)
      case (a,b) =>
        imcvazio.setVisible(false)
        (makeDouble(a), makeDouble(b)) match {
          case (Success(x), Success(y)) if x > 0 && y > 0 =>
            imcinvalido.setVisible(false)
            imcresultado.setText(Calculos.bmi(x,y).toString)
          case _ =>
            imcinvalido.setVisible(true)
        }
    }
  }

  def semanas():Unit = {
    (pesoatual.getText, pesonovo.getText) match {
      case (a, b) if a.length == 0 || b.length == 0 =>
        pesovazio.setVisible(true)
        pesoinvalido.setVisible(false)
      case (a,b) =>
        pesovazio.setVisible(false)
        (makeDouble(a), makeDouble(b)) match {
          case (Success(x), Success(y)) if x > 0 && y > 0 =>
            pesoinvalido.setVisible(false)
            pesoresultado.setText(Calculos.semanaPeso(x,y).toString)
          case _ =>
            pesoinvalido.setVisible(true)
        }
    }
  }

  def deveacordar():Unit = {
    (acordardeitahora.getText, acordardeitaminuto.getText, acordardurmiuhora.getText, acordardurmiuminuto.getText) match {
      case (a, b, c, d) if a.length == 0 || b.length == 0 || c.length == 0|| d.length == 0 =>
        acordarvazio.setVisible(true)
        acordarinvalido.setVisible(false)
        acordarresultado.setText("")
      case (a, b, c, d) =>
        acordarvazio.setVisible(false)
        (validaInputHora(a,b), validaInputHora(c,d)) match {
          case (Success(x), Success(y)) =>
            acordarinvalido.setVisible(false)
            acordarresultado.setText(Calculos.horasAcordar(x,y).toString)
          case _ =>
            acordarinvalido.setVisible(true)
            acordarresultado.setText("")
        }
    }
  }
  def devedeitar():Unit = {
    (deitardurmiuhora.getText, deitardurmiuminuto.getText, deitaracordahora.getText, deitaracordaminuto.getText) match {
      case (a, b, c, d) if a.length == 0 || b.length == 0 || c.length == 0|| d.length == 0 =>
        deitarvazio.setVisible(true)
        deitarinvalido.setVisible(false)
        deitarresultado.setText("")
      case (a, b, c, d) =>
        deitarvazio.setVisible(false)
        (validaInputHora(a,b), validaInputHora(c,d)) match {
          case (Success(x), Success(y)) =>
            deitarinvalido.setVisible(false)
            deitarresultado.setText(Calculos.horasDeitar(x,y).toString)
          case _ =>
            deitarinvalido.setVisible(true)
            deitarresultado.setText("")
        }
    }
  }
  def dormiu():Unit = {
    (dormiudeitahora.getText, dormiudeitaminuto.getText, dormiuacordahora.getText, dormiuacordaminuto.getText) match {
      case (a, b, c, d) if a.length == 0 || b.length == 0 || c.length == 0|| d.length == 0 =>
        dormiuvazio.setVisible(true)
        dormiuinvalido.setVisible(false)
        dormiuresultado.setText("")
      case (a, b, c, d) =>
        dormiuvazio.setVisible(false)
        (validaInputHora(a,b), validaInputHora(c,d)) match {
          case (Success(x), Success(y)) =>
            dormiuinvalido.setVisible(false)
            dormiuresultado.setText(Calculos.horasDormidas(x,y).toString)
          case _ =>
            dormiuinvalido.setVisible(true)
            dormiuresultado.setText("")
        }
    }
  }
}
