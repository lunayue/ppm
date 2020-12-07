package interface

import funcional.Utils.makeInt
import funcional.{Tracker, User}
import javafx.fxml.FXML
import javafx.scene.control.{Button, CheckBox, Label, TextArea, TextField}

class EditarTracker extends PassaUserTracker {
  @FXML
  var username:Button = _

  @FXML
  private var meta : TextField = _

  @FXML
  private var descricao : TextArea = _

  @FXML
  private var melhorultrapassar : CheckBox = _
  @FXML
  private var trackerpublico : CheckBox = _

  @FXML
  private var nome: Label = _

  @FXML
  private var wronginputs : Label = _
  @FXML
  private var missinginputs : Label = _

  var tracker:Tracker = _

  def initData(u:User, t:Tracker):Unit = {
    user = u
    tracker = t
    username.setText(user.nome)

    nome.setText(tracker.nome)
    meta.setText(tracker.meta.toString)
    descricao.setText(tracker.descricao)
    melhorultrapassar.setSelected(tracker.melhorUltrapassar)
    trackerpublico.setSelected(tracker.publico)
  }

  def altera():Unit = {
    val m = meta.getText
    val mi = makeInt(m)
    val d = descricao.getText
    val mu = melhorultrapassar.isSelected
    val tp = trackerpublico.isSelected

    if(m.length == 0 || d.length == 0){
      missinginputs.setVisible(true)
      wronginputs.setVisible(false)
    }
    else if(mi.isFailure || mi.get < 0){
      missinginputs.setVisible(false)
      wronginputs.setVisible(true)
    }
    else Utils.abreVerTrackers(username.getScene, user.alteraTracker(new Tracker(tracker.nome, d, mi.get, mu, tp, tracker.registos))._1)
  }
}
