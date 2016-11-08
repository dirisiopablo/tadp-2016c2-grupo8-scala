package Guerrero

import Item._
import Movimiento.Movimiento
import ResultadoPelea.{ResultadoPelea, SiguenPeleando}
import Criterio.{Criterio, MenorDesventaja}

import scala.util.Try

trait Guerrero {

  type PlanDeAtaque = Option[Seq[Movimiento]]
//  type ResultadoPelea = (Guerrero, Guerrero)

  val caracteristicas: Caracteristicas

  def movimientos = caracteristicas.movimientos

  def energia = caracteristicas.energia

  def copiarConEnergia(energia: Int): Guerrero

  def isAlive: Boolean = energia > 0

  def atacar(guerrero: Guerrero, movimiento: Movimiento): ResultadoPelea = movimiento(this, guerrero)

  /**
    * Si el resultado del criterio es igual o menor a 0 significa que el movimiento no es deseable en absoluto
    * y no debe ser considerado una respuesta válida.
  */
  def movimentoMasEfectivoContra(guerrero: Guerrero)(criterio: Criterio): Option[Movimiento] = {
    if ((movimientos map criterio.simular(this, guerrero)).forall( _ <= 0 )) None // ¯\_(ツ)_/¯
    else Some(movimientos.maxBy(criterio.simular(this, guerrero)))
  }

  /**
    * Cuando un guerrero pelea un round, realiza un movimiento (previamente elegido) contra el oponente.
    * Inmediatamente después de sufrir el efecto del movimiento, el oponente realiza un contraataque.
    * El sistema debe asumir que el oponente siempre realiza el ataque que lo deje con la mayor ventaja
    * (o al menos, con la menor desventaja) sobre los puntos de ki.
    * Al finalizar el round, el usuario debe poder tener acceso al nuevo estado del atacante y el defensor.
    */
  def pelearRound(movimiento: Movimiento, guerrero: Guerrero): (Guerrero, Guerrero) = {
    val resultado = this.atacar(guerrero, movimiento)
    resultado.elOtro.atacar(resultado.yo, resultado.elOtro.movimentoMasEfectivoContra(resultado.yo)(MenorDesventaja))
  }

  /**
    * Para cada round solicitado, el sistema debe elegir el movimiento más efectivo de acuerdo al criterio recibido
    * para realizar contra el oponente.
    * Luego debe simular la pelea de dicho round utilizando el movimiento elegido
    * y seleccionar el movimiento para el siguiente round basándose en el resultado de este.
    *
    *
    * ESTO
    * LEER ESTO -> Si el guerrero no encuentra un movimiento satisfactorio para cada round pedido,
    *              NO DEBE retornarse un plan más corto.   <- ESTO
    * ESO
    *
    *
    * BONUS: Hacerlo sin usar recursividad ni asignación destructiva!
    */
  def planDeAtaqueContra(guerrero: Guerrero, rounds: Int)(criterio: Criterio): PlanDeAtaque = {
//    type state = (Option[ResultadoPelea], PlanDeAtaque)
//
//    def nextState(s: state): state = {
//      val yo = s._1.yo
//      val elOtro = s._1.elOtro
//      val mov = yo.movimentoMasEfectivoContra(elOtro)(criterio)
//      (yo.pelearRound(mov, elOtro), s._2 :+ mov)
//    }
//
//    val seed = ((this, guerrero), Seq[Movimiento]())
//    (1 to rounds).fold(seed) { (a, _) => nextState(a) }._2

    // todo: SI MOVIMIENTO ES NONE -> ROMPE TODO QUE ESTA TODO BIEN

    val primerMovimiento = this.movimentoMasEfectivoContra(guerrero)(criterio)
    val primerResultado = pelearRound(primerMovimiento, guerrero)
    (1 until  (rounds -1)).fold(primerResultado, Seq[Option[Movimiento]](primerMovimiento)) { (res, elem) =>

    }

  }

  /**
    * Uno de los dos peleadores ganó:
    * - Esto ocurre solamente cuando el oponente muere.
    * Queremos saber quién de los dos fué y en qué estado quedó.
    * El estado del perdedor no importa.
    * *
    * Ambos siguen peleando:
    * - La pelea no tiene un ganador definido, pero queremos saber en qué estado están.
    * Para ganar, no es necesario pelear todos los rounds previstos en el plan.
    * Si uno de los peleadores muere durante un round, ese round se termina de pelear, se declara al ganador
    * y ya NO DEBEN PELEARSE LOS ROUNDS SIGUIENTES;
    * es decir, el estado del ganador es el que le quede en el round que ganó.
    * *
    * Si ambos peleadores mueren en el mismo round se considera que el ganador es el receptor del mensaje
    * (después de todo, cumplió el objetivo de su ataque...).
    * *
    *
    */
  def pelearContra(guerrero: Guerrero)(planDeAtaque: PlanDeAtaque): Try[ResultadoPelea] = {
    // PENSAR EL MAP DE RESULTADO Y SALE ARANDO
    planDeAtaque.fold(SiguenPeleando(this, guerrero)) { (s: ResultadoPelea, m: Option[Movimiento]) => s.yo.pelearRound(m, s.elOtro)}
  }

}

trait Fusionable

trait ResultadoPelea

case class SiguenPeleando(yo: Guerrero, elOtro: Guerrero) extends ResultadoPelea {
  def map(f: Guerrero => Guerrero): SiguenPeleando = SiguenPeleando(f(yo), f(elOtro))
  def filter(f: Guerrero => Boolean): SiguenPeleando = if (f(yo) && f(elOtro)) this else throw new Exception("Falló el filtrado.")
  def flatMap(f: Guerrero => Guerrero): SiguenPeleando = SiguenPeleando(f(yo), f(elOtro))
  def fold[T](e: (SiguenPeleando => T))(f: (SiguenPeleando => T)): T = f(this)
}

case class Caracteristicas(nombre: String, inventario: List[Item], movimientos: List[Movimiento], energiaMax: Int, energia: Int)

case class Humano(caracteristicas: Caracteristicas) extends Guerrero with Fusionable {
  def copiarConEnergia(energia: Int): Humano = copy(caracteristicas copy (energia = energia))
}

case class Saiyajin(caracteristicas: Caracteristicas, cola: Boolean = true, nivelSaiyajin: Int = 0, estadoMono: Boolean = false) extends Guerrero with Fusionable {
  def copiarConEnergia(energia: Int): Saiyajin = copy(caracteristicas copy (energia = energia))
}

case class Androide(caracteristicas: Caracteristicas) extends Guerrero {
  def copiarConEnergia(energia: Int): Androide = copy(caracteristicas copy (energia = energia))
}

case class Namekusein(caracteristicas: Caracteristicas) extends Guerrero with Fusionable {
  def copiarConEnergia(energia: Int): Namekusein = copy(caracteristicas copy (energia = energia))
}

case class Monstruo(caracteristicas: Caracteristicas, formaDeDigerir: (Guerrero => Guerrero)) extends Guerrero {
  def copiarConEnergia(energia: Int): Monstruo = copy(caracteristicas copy (energia = energia))
}

case class Fusionado(caracteristicas: Caracteristicas) extends Guerrero {
  def copiarConEnergia(energia: Int): Fusionado = copy(caracteristicas copy (energia = energia))
}