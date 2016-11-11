package Item

import Guerrero._


//TODO: mepa que inconsciente !== energia = 0

trait Arma extends Item {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
}

trait ArmaRoma extends Arma {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo match {
    case Androide(_) if objetivo.energia < 300 => (ejecutante, objetivo copiarConEnergia 0)
    case _ => (ejecutante, objetivo) // ¯\_(ツ)_/¯
  }
}

trait ArmaFilosa extends Arma {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo match {
    case Saiyajin(_, tieneCola, _, esMono) if tieneCola => ??? // copiar con cola false y energia = 1
    case Saiyajin(_, cola, _, esMono) if esMono => ??? // copiar con cola false, mono false e inconsciente
    case _ => (ejecutante, objetivo copiarConEnergia (objetivo.energia - ejecutante.energia / 100))
  }
}

// TODO: checkear que tenga municion disponible
trait ArmaDeFuego extends Arma {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero) = objetivo match {
    case Humano(_) => (ejecutante, objetivo copiarConEnergia (objetivo.energia - 20))
    case Namekusein(_) => (ejecutante, objetivo copiarConEnergia(objetivo.energia - 10)) // si esta inconsciente..
    case _ => (ejecutante, objetivo) // ¯\_(ツ)_/¯
  }
}

