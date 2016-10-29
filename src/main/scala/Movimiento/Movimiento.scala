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
    (ejecutante, atacado) match {
      case (Androide(_, _, _, _, _), Humano(nombre, inventario, movimientos, kiMax, ki)) =>
        (ejecutante, Humano(nombre, inventario, movimientos, kiMax, ki - 10))
      case _ if ejecutante.currentEnergy < atacado.currentEnergy =>
         (ejecutante copyWithEnergy (ejecutante.currentEnergy - 20), atacado)
      case _ if ejecutante.currentEnergy > atacado.currentEnergy =>
         (ejecutante, atacado copyWithEnergy (atacado.currentEnergy - 20))
    }
  }
}

case class Explotar() extends AtaqueFisico {
  def aplicar(ejecutante: Guerrero, atacado: Guerrero) = {
    (ejecutante, atacado) match {
      case (Androide(nombre, inventario, movimientos, bateriaMax, bateria), b) =>
        (ejecutante copyWithEnergy 0, b copyWithEnergy (b.currentEnergy - ejecutante.currentEnergy * 3))
      case (Namekusein(nombre, inventario, movimientos, kiMax, ki), b) =>
        (ejecutante copyWithEnergy 1, b copyWithEnergy (b.currentEnergy - ejecutante.currentEnergy * 2))
      case _ =>
        (ejecutante copyWithEnergy 0, atacado copyWithEnergy (atacado.currentEnergy - ejecutante.currentEnergy * 2))
    }
  }
}

