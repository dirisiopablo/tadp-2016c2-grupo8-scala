package Item

import Guerrero.Guerrero

trait Item

trait ItemNoUsable extends Item

trait ItemUsable extends Item {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
  def esArma: Boolean //FIXME: refactor plzzz
}

case object SemillaDelErmitanio extends ItemUsable {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a copiarConEnergia a.energiaMax, b)
  override def esArma: Boolean = false
}

trait Municion extends ItemNoUsable
case object BalaDeChumbo extends Municion
case object BalaDeBasoca extends Municion
case object MisilAntiaereo extends Municion

case object FotoDeLaLuna extends ItemNoUsable

case object EsferaDeUnaEstrella extends ItemNoUsable
case object EsferaDeDosEstrellas extends ItemNoUsable
case object EsferaDeTresEstrellas extends ItemNoUsable
case object EsferaDeCuatroEstrellas extends ItemNoUsable
case object EsferaDeCincoEstrellas extends ItemNoUsable
case object EsferaDeSeisEstrellas extends ItemNoUsable
case object EsferaDeSieteEstrellas extends ItemNoUsable
