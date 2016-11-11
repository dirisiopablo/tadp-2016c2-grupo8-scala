package Movimiento

import Guerrero._

trait Movimiento {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
}

trait Ataque extends Movimiento

trait AtaqueFisico extends Ataque

trait AtaqueEnergia extends Ataque

case object MuchosGolpes extends AtaqueFisico {
  // check if se murio
  def apply(ejecutante: Guerrero, atacado: Guerrero) = {
    (ejecutante, atacado) match {
      case (Androide(_), Humano(caracteristicas)) =>
        (ejecutante, atacado copiarConEnergia (atacado.energia - 10))
      case _ if ejecutante.energia < atacado.energia =>
        (ejecutante copiarConEnergia (ejecutante.energia - 20), atacado)
      case _ if ejecutante.energia > atacado.energia =>
        (ejecutante, atacado copiarConEnergia (atacado.energia - 20))
    }
  }
}

case object Explotar extends AtaqueFisico {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = {
    (ejecutante, atacado) match {
      case (Androide(caracteristicas), b) =>
        (b copiarConEnergia (b.energia - ejecutante.energia * 3), ejecutante copiarConEnergia 0)
      case (Namekusein(caracteristicas), b) =>
        (ejecutante copiarConEnergia 1, b copiarConEnergia (b.energia - ejecutante.energia * 2))
      case _ =>
        (atacado copiarConEnergia (atacado.energia - ejecutante.energia * 2), ejecutante copiarConEnergia 0)
    }
  }
}

