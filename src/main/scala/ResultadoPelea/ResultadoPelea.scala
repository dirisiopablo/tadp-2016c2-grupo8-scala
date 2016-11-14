package ResultadoPelea

import Guerrero.Guerrero

trait ResultadoPelea {
  type Guerreros = (Guerrero, Guerrero)
  def map(f: Guerrero => Guerreros): ResultadoPelea
  def filter(f: Guerreros => Boolean): ResultadoPelea
  def flatMap(f: Guerreros => ResultadoPelea): ResultadoPelea
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T
}

case class Ganador(guerrero: Guerrero) extends ResultadoPelea {
  def map(f: Guerrero => Guerreros): Ganador = this
  def filter(f: Guerreros => Boolean): ResultadoPelea = if(f(guerrero, guerrero)) this else SinPelea()
  def flatMap(f: Guerreros => ResultadoPelea): Ganador = this
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = f(this)
}

case class SiguenPeleando(guerreros: (Guerrero, Guerrero)) extends ResultadoPelea {
  def map(f: Guerrero => Guerreros): ResultadoPelea = {
    f(guerreros._2) match {
      case (atacante, atacado) if atacado.energia < 0 => Ganador(atacante)
      case (atacante, atacado) if atacante.energia < 0 => Ganador(atacado)
      case (atacante, atacado) => SiguenPeleando((atacante, atacado))
    }
  }
  def filter(f: Guerreros => Boolean): ResultadoPelea = if (f(guerreros)) this else SinPelea()
  def flatMap(f: Guerreros => ResultadoPelea) = f(guerreros)
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = f(this)
}

case class SinPelea() extends ResultadoPelea {
  def map(f: Guerrero => Guerreros): ResultadoPelea = this
  def filter(f: Guerreros => Boolean): ResultadoPelea = this
  def flatMap(f: Guerreros => ResultadoPelea): ResultadoPelea = this
  def fold[T](e: (ResultadoPelea => T))(f: (ResultadoPelea => T)): T = e(this)
}