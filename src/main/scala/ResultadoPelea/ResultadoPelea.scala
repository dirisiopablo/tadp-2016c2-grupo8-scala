package ResultadoPelea

import Guerrero.Guerrero

trait ResultadoPelea {
  def map(f: Guerrero => Guerrero): ResultadoPelea
  def filter(f: Guerrero => Boolean): ResultadoPelea
  def flatMap(f: Guerrero => ResultadoPelea): ResultadoPelea
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T
}

case class SiguenPeleando(g1: Guerrero, g2: Guerrero) extends ResultadoPelea {
  def map(f: Guerrero => Guerrero): ResultadoPelea = ???
  def filter(f: Guerrero => Boolean): ResultadoPelea = ???
  def flatMap(f: Guerrero => ResultadoPelea): ResultadoPelea = ???
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = ???
}

case class Ganador(guerrero: Guerrero) extends ResultadoPelea {
  def map(f: Guerrero => Guerrero): ResultadoPelea = this
  def filter(f: Guerrero => Boolean): ResultadoPelea = ???
  def flatMap(f: Guerrero => ResultadoPelea): ResultadoPelea = this
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = ???
}


