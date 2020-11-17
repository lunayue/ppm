package everything

case class Hora(h: Int, m: Int) {
  require(h < 24 && h >= 0)
  require(m < 60 && m >= 0)

  def +(o: Hora): Hora = {
    val auxM = m + o.m
    val nM = if (auxM > 60) auxM - 60 else auxM
    val auxH = if (nM == auxM) h + o.h else h + o.h + 1
    val nH = if (auxH > 24) auxH - 24 else auxH
    Hora(nH, nM)
  }

  def -(o: Hora): Hora = {
    val auxM = m - o.m
    val nM = if (auxM < 0) auxM + 60 else auxM
    val auxH = if (nM == auxM) h - o.h else h - o.h - 1
    val nH = if (auxH < 24) auxH + 24 else auxH
    Hora(nH, nM)
  }
}
