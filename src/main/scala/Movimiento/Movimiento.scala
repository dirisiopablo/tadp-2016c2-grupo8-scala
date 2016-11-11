package Movimiento

import Guerrero._
import Item._

trait Movimiento {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
}

case object DejarseFajar extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = (ejecutante, atacado)
}

case object Cargar extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante match {
      case Saiyajin(caracteristicas, _, nivelSaiyajin, _) if nivelSaiyajin > 0 =>
        (ejecutante copiarConEnergia (ejecutante.energia + 150 * nivelSaiyajin), atacado)
      case Androide(_) =>
        (ejecutante, atacado) // ¯\_(ツ)_/¯
      case _ =>
        (ejecutante copiarConEnergia (ejecutante.energia + 100), atacado)
  }
}

case class UsarItem(item: Item) extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = item(ejecutante, atacado)
}

case object ComerseAlOponente extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante match {
      case Monstruo(_, formaDeDigerir) if ejecutante.energia > atacado.energia =>
        (formaDeDigerir(atacado), atacado copiarConEnergia 0)
      case _ =>
        (ejecutante, atacado) // que verguenza
  }
}

case object ConvertirseEnMono extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante match {
      // falta checkear si tiene foto de la luna / hacer cuando funcione el copiar
      case Saiyajin(caracteristicas, cola, nivelSaiyajin, estadoMono) if cola && nivelSaiyajin == 0 && !estadoMono => ???
      case _ =>
        (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

case object ConvertirseEnSuperSaiyajin extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante match {
      // hacer cuando funcione el copiar
      case Saiyajin(caracteristicas, _, nivelSaiyajin, _) if ejecutante.energia * 2 > ejecutante.energiaMax => ???
      case _ =>
        (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

// este devuelve un solo guerrero D: que hacemo?
case object Fusion extends Movimiento {
  def apply(ejecutante: Guerrero, con: Guerrero) = (ejecutante, con) match {
      // hacer cuando funcione el copiar
      case (a, b) if a.isInstanceOf[Fusionable] && b.isInstanceOf[Fusionable] => ???
      case _ =>
        (ejecutante, con) // ¯\_(ツ)_/¯
  }
}

// ???????????????????????????????????????????????
case object Magia extends Movimiento {
  def apply(ejecutante: Guerrero, con: Guerrero) = ???
}
