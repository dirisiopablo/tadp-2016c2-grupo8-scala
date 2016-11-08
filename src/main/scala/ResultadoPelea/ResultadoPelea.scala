package ResultadoPelea

import Guerrero.Guerrero

trait ResultadoPelea {
  type SiguenPeleandoResult = (Guerrero, Guerrero)
  type GanadorResult = Guerrero
}

case class SiguenPeleando(guerreros: (Guerrero, Guerrero)) extends ResultadoPelea {

  def map(f: SiguenPeleandoResult => SiguenPeleandoResult): SiguenPeleando = SiguenPeleando(f(guerreros))
  def filter(f: SiguenPeleandoResult => Boolean): SiguenPeleando = if (f(guerreros)) this else throw new Exception("Falló el filtrado.")
  def flatMap(f: SiguenPeleandoResult => SiguenPeleando): SiguenPeleando = f(guerreros)
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = f(this)
}

case class Ganador(guerrero: Guerrero) extends ResultadoPelea {
  def map(f: SiguenPeleandoResult => Ganador): Ganador = this
  def filter(f: SiguenPeleandoResult => Boolean): Ganador = this
  def flatMap(f: SiguenPeleandoResult => Ganador): Ganador = this
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = f(this)
}