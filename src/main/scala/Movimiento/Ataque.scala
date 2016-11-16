package Movimiento

import Guerrero.{Androide, Guerrero, Humano, Namekusein, Monstruo}

trait Ataque extends Movimiento {
  def doApply(ejecutante: Guerrero, atacado: Guerrero): (Guerrero, Guerrero)
  def apply(ejecutante: Guerrero, atacado: Guerrero) = {
    if (ejecutante.energia == 0) (ejecutante, atacado) //TODO: validar que el guerrero no está muerto o está inconsciente
    else {
      doApply(ejecutante, atacado)
    }
  }
}

trait AtaqueFisico extends Ataque

trait AtaqueEnergia extends Ataque {
  def kiRequerido: Integer
  def doApply(ejecutante: Guerrero, atacado: Guerrero) = {
    if (ejecutante.energia <= kiRequerido) (ejecutante, atacado) //TODO: agregar || ejecutante.inconsciente
    else {
      (ejecutante, atacado) match {
          case (_, _:Androide) => (nuevoEjecutante(ejecutante), nuevoAtacado(atacado, kiRequerido) )
          case (_, _:Monstruo) => (nuevoEjecutante(ejecutante), nuevoAtacado(atacado, - kiRequerido / 2) )
          case (_, _) => (nuevoEjecutante(ejecutante), nuevoAtacado(atacado, - kiRequerido * 2) )
        }
      }
  }

  private def nuevoEjecutante(ejecutante: Guerrero): Guerrero = {
    ejecutante copiarConEnergia (ejecutante.energia - kiRequerido)
  }

  private def nuevoAtacado(atacado: Guerrero, energiaAdicional: Integer): Guerrero = {
    atacado copiarConEnergia (atacado.energia + energiaAdicional)
  }
}

case object MuchosGolpes extends AtaqueFisico {
  def doApply(ejecutante: Guerrero, atacado: Guerrero) = (ejecutante, atacado) match {

      case (Humano(_), Androide(_)) =>
        (ejecutante copiarConEnergia(ejecutante.energia - 10), atacado)

      case _ if ejecutante.energia < atacado.energia =>
        (ejecutante copiarConEnergia (ejecutante.energia - 20), atacado)

      case _ if ejecutante.energia > atacado.energia =>
        (ejecutante, atacado copiarConEnergia (atacado.energia - 20))
  }
}

case object Explotar extends AtaqueFisico {
  def doApply(ejecutante: Guerrero, atacado: Guerrero) = (ejecutante, atacado) match {

    case (Androide(_), Namekusein(_)) if atacado.energia <= ejecutante.energia * 3 =>
      (ejecutante copiarConEnergia 0, atacado copiarConEnergia 1)

    case (Monstruo(_, _), Namekusein(_)) if atacado.energia <= ejecutante.energia * 2 =>
      (ejecutante copiarConEnergia 0, atacado copiarConEnergia 1)

    case (Androide(_), _) =>
      (ejecutante copiarConEnergia 0, atacado copiarConEnergia (atacado.energia - ejecutante.energia * 3))

    case (Monstruo(_, _), _) =>
      (ejecutante copiarConEnergia 0, atacado copiarConEnergia (atacado.energia - ejecutante.energia * 2))

    case _ => (ejecutante, atacado) // ¯\_(ツ)_/¯
  }
}

case object OndaVitalTio extends AtaqueEnergia {
  override def kiRequerido: Integer = 1
}

case object Kamehameha extends AtaqueEnergia {
  override def kiRequerido: Integer = 100
}

case object FinalFlash extends AtaqueEnergia {
  override def kiRequerido: Integer = 90
}
