package funcional

import scala.annotation.tailrec
import scala.util.{Success, Try}

case class Pergunta(texto: String, opcoes: List[String], certa: Int){
  def correta(n:Int):Boolean = Pergunta.correta(this.certa, n)
  def escreve():String = Pergunta.escreve(this)

  //Apartir daqui ñ são utilizadas
  def mostra:String = Pergunta.mostra(this)
  def adicionaOpcao(opcao:String): Pergunta = Pergunta.adicionaOpcao(this, opcao)
  def opcaoValida(n:Int): Try[String] = Try(this.opcoes(n))
  def editarCorreta(n:Int): Pergunta = Pergunta.editarCorrecta(this, n)
  def editaTexto(s:String):Pergunta = Pergunta(s, this.opcoes, this.certa)
  def retiraOpcao(i:Int):Pergunta = Pergunta.retiraOpcao(this,i)
  def editarOpcao(i:Int, s:String):Pergunta = Pergunta.editarOpcao(this, i, s)
}

object Pergunta{
  def correta(c:Int, n:Int):Boolean = c == n

  def escreve(p:Pergunta):String = {
    val inicio = "Pergunta\n" + p.texto + "\n" + "Opcoes\n"
    @tailrec
    def loop(lst: List[String], s:String):(List[String], String) = lst match {
      case Nil => (List(), s)
      case a::tail => loop(tail, s + a + "\n")
    }
    inicio + loop(p.opcoes, "")._2 + "Certa\n" + p.certa + "\n"
  }


  def mostra(pergunta: Pergunta):String = {
    pergunta.texto + "\n" + (pergunta.opcoes foldRight "") (_+"\n"+_)
  }

  //Apartir daqui ñ são utilizadas
  def adicionaOpcao(pergunta: Pergunta, opcao: String):Pergunta = {
    Pergunta(pergunta.texto, pergunta.opcoes ++ List(opcao), pergunta.certa)
  }


  //Não sao puras por causa do println mas tb ñ são utilizadas pk ñ implementamos o criar quiz
  def retiraOpcao(p:Pergunta, i:Int):Pergunta = {
    p.opcaoValida(i) match {
      case Success(x) if !x.equals(p.opcoes(p.certa))=>
        lazy val aux = p.opcoes filter (y=> !y.equals(x))
        if(i < p.certa) Pergunta(p.texto, aux , p.certa-1)
        else Pergunta(p.texto, aux, p.certa)
      case _ => println("Nao e possivel retirar essa opcao")
        p
    }
  }

  def editarOpcao(p:Pergunta, i:Int, s:String):Pergunta = {
    p.opcaoValida(i) match {
      case Success(x) => Pergunta(p.texto, p.opcoes.updated(i,s) , p.certa)
      case _ => println("Nao e possivel editar essa opcao")
        p
    }
  }

  def editarCorrecta(pergunta:Pergunta, n:Int):Pergunta = {
    pergunta.opcaoValida(n) match {
      case Success(_) => Pergunta(pergunta.texto, pergunta.opcoes, n-1)
      case _ => println("Opcao invalida")
        pergunta
    }
  }
}