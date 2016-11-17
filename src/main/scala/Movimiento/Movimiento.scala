package Movimiento

import Guerrero._
import Item._
import Item.Arma

trait Movimiento {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
}

case object DejarseFajar extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = (ejecutante, atacado)
}

case object Cargar extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante match {

      case Saiyajin(caracteristicas, _, nivelSaiyajin, _) if nivelSaiyajin > 0 =>
        (ejecutante copiarConEnergia (ejecutante.energia + 150 * nivelSaiyajin min ejecutante.energiaMax), atacado)

      case Androide(_) =>
        (ejecutante, atacado) // ¯\_(ツ)_/¯

      case _ =>
        (ejecutante copiarConEnergia (ejecutante.energia + 100 min ejecutante.energiaMax), atacado)
  }
}

case class UsarItem(item: ItemUsable) extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = {
    if(ejecutante tieneItem item) {
      if (item.esArma) item(ejecutante, atacado)
      else item(ejecutante eliminarItem item, atacado)
    }
    else (ejecutante, atacado)
  }

}

case object ComerseAlOponente extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante match {

      case Monstruo(_, formaDeDigerir) if ejecutante.energia > atacado.energia =>
        (formaDeDigerir(ejecutante, atacado), atacado copiarConEnergia 0)

      case _ => (ejecutante, atacado) // que verguenza
  }
}

case object ConvertirseEnMono extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante match {

    case Saiyajin(caracteristicas, cola, nivelSaiyajin, estadoMono)
      if cola && nivelSaiyajin == 0 && !estadoMono && ejecutante.tieneItem(FotoDeLaLuna) =>

      val mono = Saiyajin(
        caracteristicas = caracteristicas.copy(
          energiaMax = ejecutante.energiaMax * 3,
          energia = ejecutante.energiaMax * 3
        ),
        cola = cola,
        nivelSaiyajin = 0,
        estadoMono = true
      )

      val monoSinLuna = mono eliminarItem FotoDeLaLuna

      (monoSinLuna, atacado)

    case _ => (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

case object ConvertirseEnSuperSaiyajin extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante match {

      case Saiyajin(caracteristicas, cola, nivelSaiyajin, estadoMono) if ejecutante.energia * 2 > ejecutante.energiaMax =>

        val ssj = Saiyajin(
          caracteristicas = caracteristicas.copy(energiaMax = ejecutante.energiaMax * 5),
          cola = cola,
          nivelSaiyajin = nivelSaiyajin + 1,
          estadoMono = estadoMono
        )

        (ssj, atacado)

      case _ =>
        (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

case class FusionarseCon(elOtro: Guerrero) extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = (ejecutante, elOtro) match {

      case (a:Fusionable, b: Fusionable)  =>

        val fusionado = Fusionado(Caracteristicas(
          nombre = a.nombre.substring(0, a.nombre.length / 2) ++ b.nombre.substring(b.nombre.length / 2),
          inventario = List(),
          movimientos = a.movimientos ++ b.movimientos,
          energiaMax = a.energiaMax + b.energiaMax,
          energia = a.energia + b.energia
        ))

        (fusionado, atacado)

      case _ => (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

case class Magia(magia: (Guerrero, Guerrero) => (Guerrero, Guerrero)) extends Movimiento {
  def apply(ejecutante: Guerrero, con: Guerrero) = magia(ejecutante, con)
}
