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
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante.tipo match {

      case Saiyajin(_, nivelSaiyajin, _, _) if nivelSaiyajin > 0 =>
        (ejecutante copiarConEnergia (ejecutante.energia + 150 * nivelSaiyajin min ejecutante.energiaMax), atacado)

      case Androide() =>
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
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante.tipo match {

      case Monstruo(formaDeDigerir, _) if ejecutante.energia > atacado.energia =>
        (formaDeDigerir(ejecutante, atacado), atacado copiarConEnergia 0)

      case _ => (ejecutante, atacado) // que verguenza
  }
}

case object ConvertirseEnMono extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante.tipo match {

    case saiyan@Saiyajin(cola, nivelSaiyajin, estadoMono, _)
      if cola && nivelSaiyajin == 0 && !estadoMono && ejecutante.tieneItem(FotoDeLaLuna) =>

      val mono = ejecutante.copy(
        caracteristicas =
          ejecutante.caracteristicas.copy(
            energiaMax = ejecutante.energiaMax * 3,
            energia = ejecutante.energiaMax * 3
          ),
        tipo =
          saiyan.copy(
            nivelSaiyajin = 0,
            estadoMono = true,
            inconsciente = false
          )
      )

      val monoSinLuna = mono eliminarItem FotoDeLaLuna

      (monoSinLuna, atacado)

    case _ => (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

case object ConvertirseEnSuperSaiyajin extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = ejecutante.tipo match {

      case saiyan@Saiyajin(cola, nivelSaiyajin, estadoMono, inconsciente) if ejecutante.energia * 2 > ejecutante.energiaMax =>
        val ssj = ejecutante.copy(
          caracteristicas = ejecutante.caracteristicas.copy(energiaMax = ejecutante.energiaMax * 5),
          tipo = saiyan.copy(
            nivelSaiyajin = nivelSaiyajin + 1
          )
        )

        (ssj, atacado)
      case _ =>
        (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

case class FusionarseCon(elOtro: Guerrero) extends Movimiento {
  def apply(ejecutante: Guerrero, atacado: Guerrero) = (ejecutante.tipo, elOtro.tipo) match {

      case (a:Fusionable, b: Fusionable)  =>

        val fusionado = ejecutante.copy(
          caracteristicas = Caracteristicas(
            nombre = ejecutante.nombre.substring(0, ejecutante.nombre.length / 2) ++ elOtro.nombre.substring(elOtro.nombre.length / 2),
            movimientos = ejecutante.movimientos ++ elOtro.movimientos,
            inventario = List(),
            energiaMax = ejecutante.energiaMax + elOtro.energiaMax,
            energia = ejecutante.energia + elOtro.energia
          ),
          tipo = Fusionado(inconsciente = false)
        )
        (fusionado, atacado)

      case _ => (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

case class Magia(magia: (Guerrero, Guerrero) => (Guerrero, Guerrero)) extends Movimiento {
  def apply(ejecutante: Guerrero, con: Guerrero) = magia(ejecutante, con)
}
