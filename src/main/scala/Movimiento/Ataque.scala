package Movimiento

import Guerrero.{Androide, Guerrero, Humano, Namekusein, Monstruo}

trait Ataque extends Movimiento
trait AtaqueFisico extends Ataque
trait AtaqueEnergia extends Ataque // ver como centralizar la absorcion de los androides

case object MuchosGolpes extends AtaqueFisico {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = (ejecutante, atacado) match {
      case (Androide(_), Humano(caracteristicas)) =>
        (ejecutante, atacado copiarConEnergia (atacado.energia - 10))
      case _ if ejecutante.energia < atacado.energia =>
        (ejecutante copiarConEnergia (ejecutante.energia - 20), atacado)
      case _ if ejecutante.energia > atacado.energia =>
        (ejecutante, atacado copiarConEnergia (atacado.energia - 20))
  }
}

case object Explotar extends AtaqueFisico {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = (ejecutante, atacado) match {
      case (Androide(caracteristicas), b) =>
        (b copiarConEnergia (b.energia - ejecutante.energia * 3), ejecutante copiarConEnergia 0)
      case (Namekusein(caracteristicas), b) => // FIXME
        (ejecutante copiarConEnergia 1, b copiarConEnergia (b.energia - ejecutante.energia * 2))
      case _ if ejecutante.isInstanceOf[Androide] || ejecutante.isInstanceOf[Monstruo] =>
        (atacado copiarConEnergia (atacado.energia - ejecutante.energia * 2), ejecutante copiarConEnergia 0)
      case _ =>
        (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

// TODO
case class Onda() extends AtaqueEnergia {}