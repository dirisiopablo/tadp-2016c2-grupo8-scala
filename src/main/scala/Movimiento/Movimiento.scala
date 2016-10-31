package Movimiento

import Guerrero.{Androide, Guerrero, Humano, Namekusein}

trait Movimiento {
  type ResultadoPelea = (Guerrero, Guerrero)
  def aplicar(ejecutante: Guerrero, objetivo: Guerrero): ResultadoPelea
}

trait Ataque extends Movimiento

trait AtaqueFisico extends Ataque

trait AtaqueEnergia extends Ataque

case object MuchosGolpes extends AtaqueFisico {

  def aplicar(ejecutante: Guerrero, atacado: Guerrero) = {
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
  def aplicar(ejecutante: Guerrero, atacado: Guerrero) = {
    (ejecutante, atacado) match {
      case (Androide(caracteristicas), b) =>
        (ejecutante copiarConEnergia 0, b copiarConEnergia (b.energia - ejecutante.energia * 3))
      case (Namekusein(caracteristicas), b) =>
        (ejecutante copiarConEnergia 1, b copiarConEnergia (b.energia - ejecutante.energia * 2))
      case _ =>
        (ejecutante copiarConEnergia 0, atacado copiarConEnergia (atacado.energia - ejecutante.energia * 2))
    }
  }
}

