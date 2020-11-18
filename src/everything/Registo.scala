package everything

case class Registo(data:String, dado:Int, meta:Int, melhorUltrapassar:Boolean){
  def alcancado():Boolean = Registo.alcancado(this)
  def muda(novo:Int):Registo = Registo(this.data, novo, this.meta, this.melhorUltrapassar)
  def escreve():String = Registo.escreve(this)
  def verRegisto():String = Registo.verRegisto(this)
}

object Registo{
  def alcancado(r: Registo):Boolean = if(r.melhorUltrapassar) r.dado >= r.meta else r.dado <= r.meta
  def escreve(r:Registo):String = r.data + "\n" + r.dado + "\n" + r.meta + "\n" + r.melhorUltrapassar + "\n"
  def verRegisto(r:Registo):String = r.data + ") " + r.dado + "-> Meta " + (if (r.alcancado()) "alcancada" else "nao alcancada") + "\n"
}
