package Item

import Guerrero.Guerrero

trait Item {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
}

case object SemillaDelErmitanio extends Item {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a copiarConEnergia a.energiaMax, b)
}