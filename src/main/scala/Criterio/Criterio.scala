package Criterio

import Guerrero.Guerrero
import Movimiento.Movimiento

/**
  * Los criterios pueden ser de lo más variados:
  * - usar el movimiento que más daño le haga al enemigo
  * - usar el movimiento que deje a su enemigo con la mayor cantidad de ki
  * - usar los movimientos que les hagan perder la menor cantidad de ítems
  * - usar cualquier movimiento que no lo mate.
  **
  * Para simplificar la decisión, se pide modelar los criterios como cuantificadores del resultado de realizar el movimiento;
  * es decir, un criterio debe procesar el estado del ejecutante y el defensor luego de realizar el movimiento
  * y producir un número que represente qué tan “deseado” es dicho resultado.
  * Cuanto más grande sea el número, más favorecido será el movimiento analizado.
  **
  * Es importante tener en cuenta que el guerrero podría no disponer de ningún movimiento que satisfaga el criterio,
  * lo cual debe ser manejado de forma acorde.
  * En caso de que el criterio produzca el mismo valor para más de un movimiento, se puede elegir cualquier de ellos.
  */

trait Criterio {
  def simular(ejecutante: Guerrero, atacado: Guerrero)(movimiento: Movimiento): Int
}

// mayor daño al otro
case object MayorDanio extends Criterio {
  override def simular(ejecutante: Guerrero, atacado: Guerrero)(movimiento: Movimiento) = {
    val res = movimiento(ejecutante, atacado)
    atacado.energia - res.elOtro.energia
  }

}

// menor daño al otro
case object MenorDanio extends Criterio {
  override def simular(ejecutante: Guerrero, atacado: Guerrero)(movimiento: Movimiento) = {
    val res = movimiento(ejecutante, atacado)
    res.elOtro.energia - atacado.energia
  }
}

// menor diferencia de ki entre los 2
case object MenorDesventaja extends Criterio {
  override def simular(ejecutante: Guerrero, atacado: Guerrero)(movimiento: Movimiento) = {
    val res = movimiento(ejecutante, atacado)
    math.abs(res.yo.energia - res.elOtro.energia)
  }
}

// el que gaste menos items del ejecutante
case object GastarMenosItems extends Criterio {
  override def simular(ejecutante: Guerrero, atacado: Guerrero)(movimiento: Movimiento) = {
    val res = movimiento(ejecutante, atacado)
    ejecutante.caracteristicas.inventario.length - res.yo.caracteristicas.inventario.length
  }
}

// cualquier cosa que no deje al ejecutante en 0
case object QueNoMeMate extends Criterio {
  override def simular(ejecutante: Guerrero, atacado: Guerrero)(movimiento: Movimiento) = {
    val res = movimiento(ejecutante, atacado)
    res.yo.energia
  }
}