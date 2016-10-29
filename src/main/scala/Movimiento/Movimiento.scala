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
      case (a, b) =>
//        if (a.currentEnergy() < b.currentEnergy()) (a, b) //TODO: devolver al guerrero con ki -= 20
//        else (a, b) //TODO: devolver al atacado con ki -= 20

    }
  }
}

case class Explotar() extends AtaqueFisico {
  def aplicar(ejecutante: Guerrero, atacado: Guerrero) = {
    (ejecutante, atacado) match {
      case (Androide(nombre, inventario, movimientos, bateriaMax, bateria), b) => //TODO: androide con bateria = 0, b con ki/bat - bateria*3
      case (Namekusein(nombre, inventario, movimientos, kiMax, ki), b) => //TODO: namek ki=1 el otro con ki - namek ki * 2
      case _ => //TODO: ejecutante ki = 0, atacante ki - ej.ki *2
    }
  }
}

