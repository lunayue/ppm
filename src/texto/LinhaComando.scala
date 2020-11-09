package texto
import users._

class LinhaComando[A](val nome:String, val exec: A=>A)

class LCL(val nome:String, val exec: Option[User])
