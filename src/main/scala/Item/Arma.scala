package Item

import Guerrero._

trait Arma extends ItemUsable {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
  override def esArma: Boolean = true
}

trait ArmaRoma extends Arma {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo match {
    case objetivo: Inconscientable if objetivo.energia < 300 => (ejecutante, objetivo.copiarInconsciente)
    case _ => (ejecutante, objetivo) // ¯\_(ツ)_/¯
  }
}

trait ArmaFilosa extends Arma {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo match {

    case Saiyajin(c, true, n, false, i) =>

//      val saiyan = Saiyajin(
//        caracteristicas = c.copy(energia = 1),
//        cola = false,
//        nivelSaiyajin = n,
//        estadoMono = false,
//        inconsciente = i
//      )

      val saiyan = ejecutante.asInstanceOf[Saiyajin]
        .copy(caracteristicas = c.copy(energia = 1), cola = false)

      (ejecutante, saiyan)

    case Saiyajin(c, _, n, true, i) =>

      val saiyan = Saiyajin(
        caracteristicas = c,
        cola = false,
        nivelSaiyajin = n,
        estadoMono = false,
        inconsciente = true
      )

      (ejecutante, saiyan)

    case _ => (ejecutante, objetivo copiarConEnergia (objetivo.energia - ejecutante.energia / 100))
  }
}

trait ArmaDeFuego extends Arma {
  val municionRequerida: Municion
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo match {
    case _ if !ejecutante.tieneItem(municionRequerida) => (ejecutante, objetivo)
    case Humano(_, _) => (ejecutante eliminarItem municionRequerida, objetivo copiarConEnergia (objetivo.energia - 20))
    case Namekusein(_, true) => (ejecutante eliminarItem municionRequerida, objetivo copiarConEnergia(objetivo.energia - 10))
    case _ => (ejecutante eliminarItem municionRequerida, objetivo) // ¯\_(ツ)_/¯
  }
}

case object BraveSword extends ArmaFilosa

case object Chumbo extends ArmaDeFuego {
  override val municionRequerida: Municion = BalaDeChumbo
}

case object Roma extends ArmaRoma
