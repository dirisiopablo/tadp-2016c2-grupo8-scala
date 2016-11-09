package ResultadoPelea

import Guerrero.Guerrero

trait ResultadoPelea {
  type Guerreros = (Guerrero, Guerrero)
  def map(f: Guerrero => Guerreros): ResultadoPelea
  def filter(f: Guerreros => Boolean): ResultadoPelea
  def flatMap(f: Guerreros => ResultadoPelea): ResultadoPelea
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T
}

case class SiguenPeleando(guerreros: (Guerrero, Guerrero)) extends ResultadoPelea {
  def map(f: Guerrero => Guerreros): ResultadoPelea = SiguenPeleando(f(guerreros._2))
  def filter(f: Guerreros => Boolean): ResultadoPelea = if (f(guerreros)) this else throw new Exception("Falló el filtrado.")
  def flatMap(f: Guerreros => ResultadoPelea) = f(guerreros)
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = f(this)
}

case class Ganador(guerrero: Guerrero) extends ResultadoPelea {
  def map(f: Guerrero => Guerreros): ResultadoPelea = this
  def filter(f: Guerreros => Boolean): ResultadoPelea = this
  def flatMap(f: Guerreros => ResultadoPelea): ResultadoPelea = this
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = f(this)
}