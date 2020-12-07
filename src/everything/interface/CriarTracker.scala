package interface

import funcional.{Tracker, User}
import funcional.Utils.makeInt
import javafx.fxml.FXML
import javafx.scene.control.{Button, CheckBox, Label, TextArea, TextField}

class CriarTracker extends MainPage {
  @FXML
  private var username : Button = _

  @FXML
  private var nome : TextField = _
  @FXML
  private var meta : TextField = _

  @FXML
  private var descricao : TextArea = _

  @FXML
  private var melhorultrapassar : CheckBox = _
  @FXML
  private var trackerpublico : CheckBox = _

  @FXML
  private var wronginputs : Label = _
  @FXML
  private var missinginputs : Label = _
  @FXML
  private var nometaken : Label = _

  override def initData(u:User):Unit = {
    user = u
    username.setText(user.nome)
  }

  def criartrackerclicked():Unit = {
    val n = nome.getText
    val m = meta.getText
    val mi = makeInt(m)
    val d = descricao.getText
    val mu = melhorultrapassar.isSelected
    val tp = trackerpublico.isSelected

    if(n.length == 0 || m.length == 0 || d.length == 0){
      missinginputs.setVisible(true)
      wronginputs.setVisible(false)
      nometaken.setVisible(false)
    }
    else if(mi.isFailure || mi.get < 0){
      missinginputs.setVisible(false)
      wronginputs.setVisible(true)
      nometaken.setVisible(false)
    }
    else{
      user.adicionaTracker(new Tracker(n, d, mi.get, mu, tp, List())) match {
        case (_,Some(_)) => missinginputs.setVisible(false)
          wronginputs.setVisible(false)
          nometaken.setVisible(true)
        case (x, None) => Utils.abreVerTrackers(username.getScene, x)
      }
    }

  }
}
