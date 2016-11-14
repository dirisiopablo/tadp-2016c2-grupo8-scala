package Movimiento

import Guerrero.{Androide, Guerrero, Humano, Namekusein, Monstruo}

trait Ataque extends Movimiento
trait AtaqueFisico extends Ataque
trait AtaqueEnergia extends Ataque //TODO: ver como centralizar la absorcion de los androides

case object MuchosGolpes extends AtaqueFisico {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = (ejecutante, atacado) match {

      case (Humano(_), Androide(_)) =>
        (ejecutante copiarConEnergia(ejecutante.energia - 10), atacado)

      case _ if ejecutante.energia < atacado.energia =>
        (ejecutante copiarConEnergia (ejecutante.energia - 20), atacado)

      case _ if ejecutante.energia > atacado.energia =>
        (ejecutante, atacado copiarConEnergia (atacado.energia - 20))
  }
}

case object Explotar extends AtaqueFisico {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = (ejecutante, atacado) match {

    case (Androide(_), Namekusein(_)) if atacado.energia <= ejecutante.energia * 3 =>
      (ejecutante copiarConEnergia 0, atacado copiarConEnergia 1)

    case (Monstruo(_, _), Namekusein(_)) if atacado.energia <= ejecutante.energia * 2 =>
      (ejecutante copiarConEnergia 0, atacado copiarConEnergia 1)

    case (Androide(_), _) =>
      (ejecutante copiarConEnergia 0, atacado copiarConEnergia (atacado.energia - ejecutante.energia * 3))

    case (Monstruo(_, _), _) =>
      (ejecutante copiarConEnergia 0, atacado copiarConEnergia (atacado.energia - ejecutante.energia * 2))

    case _ => (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

// TODO
case class Onda() extends AtaqueEnergia {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = ???
}