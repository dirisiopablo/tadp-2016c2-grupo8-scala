package Item

import Guerrero._

trait Arma extends ItemUsable {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
  override def esArma: Boolean = true
}

trait ArmaRoma extends Arma {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo match {
    case objetivo: Inconscientable if objetivo.energia < 300 => (ejecutante, objetivo.copy(tipo = objetivo.copiarInconsciente))
    case _ => (ejecutante, objetivo) // ¯\_(ツ)_/¯
  }
}

trait ArmaFilosa extends Arma {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = {
    objetivo.tipo match {
      case saiyajin@Saiyajin(true, n, false, i) =>
        val saiyan = objetivo.copy(caracteristicas = ejecutante.caracteristicas.copy(energia = 1), tipo = saiyajin.copy(cola = false))

        (ejecutante, saiyan)

      case saiyajin@Saiyajin(_, n, true, i) =>
        val saiyan = objetivo.copy(tipo = saiyajin.copy(estadoMono = false, inconsciente = true, cola = false))
        (ejecutante, saiyan)

      case _ => (ejecutante, objetivo copiarConEnergia (objetivo.energia - ejecutante.energia / 100))
    }
  }
}

trait ArmaDeFuego extends Arma {
  val municionRequerida: Municion
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo.tipo match {
    case _ if !ejecutante.tieneItem(municionRequerida) => (ejecutante, objetivo)
    case Humano(_) => (ejecutante eliminarItem municionRequerida, objetivo copiarConEnergia (objetivo.energia - 20))
    case Namekusein(true) => (ejecutante eliminarItem municionRequerida, objetivo copiarConEnergia(objetivo.energia - 10))
    case _ => (ejecutante eliminarItem municionRequerida, objetivo) // ¯\_(ツ)_/¯
  }
}

case object BraveSword extends ArmaFilosa

case object Chumbo extends ArmaDeFuego {
  override val municionRequerida: Municion = BalaDeChumbo
}

case object Roma extends ArmaRoma
