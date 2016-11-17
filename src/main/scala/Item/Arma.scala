package Item

import Guerrero._

trait Arma extends ItemUsable {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
  override def esArma: Boolean = true
}

trait ArmaRoma extends Arma {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo match {
    case Androide(_) if objetivo.energia < 300 => (ejecutante, objetivo copiarConEnergia 0) // TODO: no energia 0 -> inconsciente
    case _ => (ejecutante, objetivo) // ¯\_(ツ)_/¯
  }
}

trait ArmaFilosa extends Arma {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo match {

    case Saiyajin(c, true, n, false) =>

      val saiyan = Saiyajin(
        caracteristicas = c.copy(energia = 1),
        cola = false,
        nivelSaiyajin = n,
        estadoMono = false
      )

      (ejecutante, saiyan)

    case Saiyajin(c, _, n, true) => // TODO agregar inconsciente

      val saiyan = Saiyajin(
        caracteristicas = c,
        cola = false,
        nivelSaiyajin = n,
        estadoMono = false
      )

      (ejecutante, saiyan)

    case _ => (ejecutante, objetivo copiarConEnergia (objetivo.energia - ejecutante.energia / 100))
  }
}

trait ArmaDeFuego extends Arma {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo match {
    case _ if !ejecutante.tieneItem(Municion) => (ejecutante, objetivo)
    case Humano(_) => (ejecutante eliminarItem Municion, objetivo copiarConEnergia (objetivo.energia - 20))
    case Namekusein(_) => (ejecutante eliminarItem Municion, objetivo copiarConEnergia(objetivo.energia - 10)) // TODO si esta inconsciente..
    case _ => (ejecutante, objetivo) // ¯\_(ツ)_/¯
  }
}

case object BraveSword extends ArmaFilosa
case object Chumbo extends ArmaDeFuego
case object Roma extends ArmaRoma
