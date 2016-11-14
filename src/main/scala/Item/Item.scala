package Item

import Guerrero.Guerrero

trait Item {
  def apply(ejecutante: Guerrero, objetivo: Guerrero): (Guerrero, Guerrero)
}

case object SemillaDelErmitanio extends Item {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a copiarConEnergia a.energiaMax, b)
}

case object FotoDeLaLuna extends Item {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a, b)
}

case object EsferaDeUnaEstrella extends Item {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a, b)
}

case object EsferaDeDosEstrellas extends Item {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a, b)
}

case object EsferaDeTresEstrellas extends Item {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a, b)
}

case object EsferaDeCuatroEstrellas extends Item {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a, b)
}

case object EsferaDeCincoEstrellas extends Item {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a, b)
}

case object EsferaDeSeisEstrellas extends Item {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a, b)
}

case object EsferaDeSieteEstrellas extends Item {
  def apply(a: Guerrero, b: Guerrero): (Guerrero, Guerrero) = (a, b)
}
