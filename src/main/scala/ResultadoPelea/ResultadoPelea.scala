package ResultadoPelea

import Guerrero.Guerrero

trait ResultadoPelea {
  type Guerreros = (Guerrero, Guerrero)
  def map(f: Guerreros => Guerreros): ResultadoPelea
  def filter(f: Guerreros => Boolean): ResultadoPelea
  def flatMap(f: Guerreros => ResultadoPelea): ResultadoPelea
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T
}

case class SiguenPeleando(guerreros: (Guerrero, Guerrero)) extends ResultadoPelea {
  def map(f: Guerreros => Guerreros): SiguenPeleando = SiguenPeleando(f(guerreros))
  def filter(f: Guerreros => Boolean): SiguenPeleando = if (f(guerreros)) this else throw new Exception("Falló el filtrado.")
  def flatMap(f: Guerreros => ResultadoPelea) = f(guerreros)
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = f(this)
}

case class Ganador(guerrero: Guerrero) extends ResultadoPelea {
  def map(f: Guerreros => Guerreros): Ganador = this
  def filter(f: Guerreros => Boolean): Ganador = this
  def flatMap(f: Guerreros => ResultadoPelea): Ganador = this
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = f(this)
}