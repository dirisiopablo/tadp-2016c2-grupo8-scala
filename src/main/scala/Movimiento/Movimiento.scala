package Movimiento

import ResultadoPelea._
import Guerrero._

trait Movimiento {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): ResultadoPelea
}

trait Ataque extends Movimiento

trait AtaqueFisico extends Ataque

trait AtaqueEnergia extends Ataque

case object MuchosGolpes extends AtaqueFisico {

  // check if se murio
  def apply(ejecutante: Guerrero, atacado: Guerrero) = {
    (ejecutante, atacado) match {
      case (Androide(_), Humano(caracteristicas)) =>
        SiguenPeleando(ejecutante, atacado copiarConEnergia (atacado.energia - 10))
      case _ if ejecutante.energia < atacado.energia =>
        SiguenPeleando(ejecutante copiarConEnergia (ejecutante.energia - 20), atacado)
      case _ if ejecutante.energia > atacado.energia =>
        SiguenPeleando(ejecutante, atacado copiarConEnergia (atacado.energia - 20))
    }
  }
}

case object Explotar extends AtaqueFisico {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = {
    (ejecutante, atacado) match {
      case (Androide(caracteristicas), b) =>
        Ganador(b copiarConEnergia (b.energia - ejecutante.energia * 3))
      case (Namekusein(caracteristicas), b) =>
        SiguenPeleando(ejecutante copiarConEnergia 1, b copiarConEnergia (b.energia - ejecutante.energia * 2))
      case _ =>
        Ganador(atacado copiarConEnergia (atacado.energia - ejecutante.energia * 2))
    }
  }
}

