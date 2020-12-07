package interface

import funcional.Utils.makeInt
import funcional.{Registo, Tracker, User}
import javafx.fxml.FXML
import javafx.scene.Scene
import javafx.scene.control.{Label, TextField}

import scala.util.Success

class EditarRegisto extends PassaUserTrackerRegisto {
  @FXML
  var novovalor: TextField = _

  @FXML
  var campovazio: Label = _
  @FXML
  var valorinvalido: Label = _

  var user:User = _
  var tracker:Tracker = _
  var registo:Registo = _
  var scene:Scene = _

  def initData(u:User, t:Tracker, r:Registo, s:Scene):Unit = {
    user = u
    tracker = t
    registo = r
    scene = s
  }

  def cancela():Unit = campovazio.getScene.getWindow.hide

  def validaValor(s:String):Option[Int] = {
    valorinvalido.setVisible(false)
    if(s.length == 0) {
      valorinvalido.setVisible(false)
      campovazio.setVisible(true)
      None
    }
    else {
      campovazio.setVisible(false)
      makeInt(s) match{
        case Success(x) if x >= 0 => Some(x)
        case _ => valorinvalido.setVisible(true)
          None
      }
    }
  }

  def substitui():Unit = {
    val novo = validaValor(novovalor.getText)
    if(novo.isDefined){
      val novoT = tracker.editarRegisto(registo.data, novo.get,(_,b)=>b)._1
      Utils.novaScenaComTracker(scene,user.alteraTracker(novoT)._1, novoT, "vertrackerespecifico.fxml")
      novovalor.getScene.getWindow.hide
    }
  }

  def adiciona():Unit = {
    val novo = validaValor(novovalor.getText)
    if(novo.isDefined){
      val novoT = tracker.editarRegisto(registo.data, novo.get,(a,b)=>a+b)._1
      Utils.novaScenaComTracker(scene,user.alteraTracker(novoT)._1, novoT, "vertrackerespecifico.fxml")
      novovalor.getScene.getWindow.hide
    }
  }

  def remove():Unit = {
    val novo = validaValor(novovalor.getText)
    if(novo.isDefined && novo.get < registo.dado){
      val novoT = tracker.editarRegisto(registo.data, novo.get,(a,b)=>a-b)._1
      Utils.novaScenaComTracker(scene,user.alteraTracker(novoT)._1, novoT, "vertrackerespecifico.fxml")
      novovalor.getScene.getWindow.hide
    }
    else if(novo.isDefined){valorinvalido.setVisible(true)}
  }

}
