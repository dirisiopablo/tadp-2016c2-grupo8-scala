package Guerrero

import Item._
import Movimiento.Movimiento
import Criterio.{Criterio, MenorDesventaja}

trait Guerrero {

  type PlanDeAtaque = Seq[Movimiento]
  type ResultadoPelea = (Guerrero, Guerrero)

  val caracteristicas: Caracteristicas

  def movimientos = caracteristicas.movimientos

  def energia = caracteristicas.energia

  def copiarConEnergia(energia: Int): Guerrero

  def atacar(guerrero: Guerrero, movimiento: Movimiento): ResultadoPelea = movimiento.aplicar(this, guerrero)

  def movimentoMasEfectivoContra(guerrero: Guerrero)(criterio: Criterio): Movimiento =
    movimientos.maxBy(criterio.simular(this, guerrero))

  /**
    * Cuando un guerrero pelea un round, realiza un movimiento (previamente elegido) contra el oponente.
    * Inmediatamente después de sufrir el efecto del movimiento, el oponente realiza un contraataque.
    * El sistema debe asumir que el oponente siempre realiza el ataque que lo deje con la mayor ventaja
    * (o al menos, con la menor desventaja) sobre los puntos de ki.
    * Al finalizar el round, el usuario debe poder tener acceso al nuevo estado del atacante y el defensor.
    */
  def pelearRound(movimiento: Movimiento)(guerrero: Guerrero): ResultadoPelea = {
    val (yo, elOtro) = this.atacar(guerrero, movimiento)
    elOtro.atacar(yo, elOtro.movimentoMasEfectivoContra(yo)(MenorDesventaja))
  }

  /**
    * Para cada round solicitado, el sistema debe elegir el movimiento más efectivo de acuerdo al criterio recibido
    * para realizar contra el oponente.
    * Luego debe simular la pelea de dicho round utilizando el movimiento elegido
    * y seleccionar el movimiento para el siguiente round basándose en el resultado de este.
    * Si el guerrero no encuentra un movimiento satisfactorio para cada round pedido, NO DEBE retornarse un plan más corto.
    *
    * BONUS: Hacerlo sin usar recursividad ni asignación destructiva!
    */
  def planDeAtaquecontra(guerrero: Guerrero, rounds: Int)(criterio: Criterio): PlanDeAtaque = {

    type state = (ResultadoPelea, PlanDeAtaque)
    def nextState(s: state): state = {
      val yo = s._1._1
      val elOtro = s._1._2
      val mov = yo.movimentoMasEfectivoContra(elOtro)(criterio)
      (yo.pelearRound(mov)(elOtro), s._2 :+ mov)
    }

    val seed = ((this, guerrero), Seq[Movimiento]())
    (1 to rounds).foldLeft(seed) { (a, _) => nextState(a) }._2
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
  def pelearContra(guerrero: Guerrero)(planDeAtaque: PlanDeAtaque): ResultadoPelea = {
    //     si el bonus era para el punto 3, cambiar el fold por una recursion con pattern matching
    //     para sacar ese return de mierda
    val seed: ResultadoPelea = (this, guerrero)
    planDeAtaque.foldLeft(seed) { (res: ResultadoPelea, m: Movimiento) =>
      if (res._1.caracteristicas.energia <= 0 || res._2.caracteristicas.energia <= 0) return res
      res._1.pelearRound(m)(res._2)
    }
  }

}

trait Fusionable

case class ResultadoPelea() {
  def map(f: Guerrero => Guerrero): ResultadoPelea = ???
  def filter(f: Guerrero => Boolean): ResultadoPelea = ???
  def flatMap(f: Guerrero => Guerrero): ResultadoPelea = ???
  def fold = ???
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