package Movimiento

import Guerrero.ResultadoPelea
import Guerrero.{Androide, Guerrero, Humano, Namekusein}

trait Movimiento {
  def aplicar(ejecutante: Guerrero, objetivo: Guerrero): ResultadoPelea
}

trait Ataque extends Movimiento

trait AtaqueFisico extends Ataque

trait AtaqueEnergia extends Ataque

case object MuchosGolpes extends AtaqueFisico {

  def aplicar(ejecutante: Guerrero, atacado: Guerrero) = {
    (ejecutante, atacado) match {
      case (Androide(_), Humano(caracteristicas)) =>
        ResultadoPelea(ejecutante, atacado copiarConEnergia (atacado.energia - 10))
      case _ if ejecutante.energia < atacado.energia =>
        ResultadoPelea(ejecutante copiarConEnergia (ejecutante.energia - 20), atacado)
      case _ if ejecutante.energia > atacado.energia =>
        ResultadoPelea(ejecutante, atacado copiarConEnergia (atacado.energia - 20))
    }
  }
}

case object Explotar extends AtaqueFisico {
  def aplicar(ejecutante: Guerrero, atacado: Guerrero) = {
    (ejecutante, atacado) match {
      case (Androide(caracteristicas), b) =>
        ResultadoPelea(ejecutante copiarConEnergia 0, b copiarConEnergia (b.energia - ejecutante.energia * 3))
      case (Namekusein(caracteristicas), b) =>
        ResultadoPelea(ejecutante copiarConEnergia 1, b copiarConEnergia (b.energia - ejecutante.energia * 2))
      case _ =>
        ResultadoPelea(ejecutante copiarConEnergia 0, atacado copiarConEnergia (atacado.energia - ejecutante.energia * 2))
    }
  }
}

