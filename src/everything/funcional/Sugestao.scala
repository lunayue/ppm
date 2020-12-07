package funcional

case class Sugestao(user:String, titulo:String, texto:String){
  def mostra():String = this.titulo + " por " + this.user + ") " +this.texto + "\n"
}
