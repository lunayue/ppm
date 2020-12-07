package funcional

case class Ajuda(pergunta:String, resposta:String){
  def mostra():String = this.pergunta + ") " +this.resposta + "\n"
}
