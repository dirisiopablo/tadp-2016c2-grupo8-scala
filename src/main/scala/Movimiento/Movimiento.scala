package Movimiento

import Guerrero.{Androide, Guerrero, Humano, Namekusein}

trait Movimiento {
  type ResultadoPelea = (Guerrero, Guerrero)
  def aplicar(ejecutante: Guerrero, objetivo: Guerrero): ResultadoPelea
}

trait Ataque extends Movimiento
trait AtaqueFisico extends Ataque
trait AtaqueEnergia extends Ataque

case class MuchosGolpes() extends AtaqueFisico {
  def aplicar(ejecutante: Guerrero, atacado: Guerrero) = {
    atacado match {
      case Androide(_) =>
        ejecutante match {
          case Humano() => (ejecutante, atacado)//TODO: devolver al nuevo humano con ki -= 10
          case _ =>
            if (ejecutante.ki < atacado.ki) (ejecutante, atacado) //TODO: devolver al guerrero con ki -= 20
            else (ejecutante, atacado) //TODO: devolver al atacado con ki -= 20
        }
      case _ =>
        if (ejecutante.ki < atacado.ki) (ejecutante, atacado) //TODO: devolver al guerrero con ki -= 20
        else (ejecutante, atacado) //TODO: devolver al atacado con ki -= 20
    }
  }
}

case class Explotar() extends AtaqueFisico {
  def aplicar(ejecutante: Guerrero, atacado: Guerrero) = {
    atacado match {
      case Androide(_) => (ejecutante, atacado)
      case Namekusein() => (ejecutante, atacado)
      case _ =>
        if (ejecutante.ki < atacado.ki) (ejecutante, atacado) //TODO: devolver al guerrero con ki -= 20
        else (ejecutante, atacado) //TODO: devolver al atacado con ki -= 20
    }
  }
}

