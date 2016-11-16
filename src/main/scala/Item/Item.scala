package Item

import Guerrero.Guerrero

trait Item

trait ItemNoUsable extends Item

trait ItemUsable extends Item {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
}

case object SemillaDelErmitanio extends ItemUsable {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a copiarConEnergia a.energiaMax, b)
}

case object Municion extends ItemNoUsable

case object FotoDeLaLuna extends ItemNoUsable

case object EsferaDeUnaEstrella extends ItemNoUsable
case object EsferaDeDosEstrellas extends ItemNoUsable
case object EsferaDeTresEstrellas extends ItemNoUsable
case object EsferaDeCuatroEstrellas extends ItemNoUsable
case object EsferaDeCincoEstrellas extends ItemNoUsable
case object EsferaDeSeisEstrellas extends ItemNoUsable
case object EsferaDeSieteEstrellas extends ItemNoUsable
