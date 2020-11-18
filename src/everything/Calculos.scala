package everything

object Calculos {
  def aguaMl(idade:Int, peso:Double, exercicio:Double):Int = idade match {
    case x if x<18 => (40*peso + 500*exercicio).toInt
    case y if y<55 => (35*peso + 500*exercicio).toInt
    case z if z<65 => (30*peso + 500*exercicio).toInt
    case _ => (25*peso + 500*exercicio).toInt
  }

  def aguaCopos(agua:Int, tamanhoCopo:Int):Int = agua/tamanhoCopo

  def bmi(m:Double, a:Double):Double = m/(a*a)

  def poupanca(t:Tracker):Int = (t.registos map (x=> x.meta-x.dado) foldRight 0 )(_+_)
  def ganho(t:Tracker):Int = (t.registos map (x=> x.dado-x.meta) foldRight 0 )(_+_)
  def total(t:Tracker):Int = (t.registos foldRight 0 )(_.dado +_)

  def semanaPeso(atual:Double, pretendido:Double):Int = (pretendido-atual).toInt

  def horasDormidas(deitar:Hora, levantar:Hora):Hora = levantar-deitar
  def horasAcordar(deitar:Hora, dormidas:Hora):Hora = deitar+dormidas
  def horasDeitar(dormidas:Hora, levantar:Hora):Hora = levantar-dormidas
}
