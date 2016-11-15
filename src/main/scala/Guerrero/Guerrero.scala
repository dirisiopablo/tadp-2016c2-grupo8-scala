package Guerrero

import Item._
import Movimiento.Movimiento
import ResultadoPelea.{ResultadoPelea, SiguenPeleando}
import Criterio.{Criterio, MenorDesventaja}

trait Guerrero {

  type PlanDeAtaque = Option[Seq[Movimiento]]

  val caracteristicas: Caracteristicas
  def movimientos = caracteristicas.movimientos

  def nombre = caracteristicas.nombre

  def energia = caracteristicas.energia
  def energiaMax = caracteristicas.energiaMax

  def itemList = caracteristicas.inventario
  def tieneItem(item: Item) = itemList contains item

  def copiarConEnergia(energia: Int): Guerrero
  def copiarConItems(items: List[Item]): Guerrero
  def copiarConMovimientos(movimientos: List[Movimiento]): Guerrero

  def atacar(guerrero: Guerrero, movimiento: Movimiento): (Guerrero, Guerrero) = movimiento(this, guerrero)

  /**
    * Si el resultado del criterio es igual o menor a 0 significa que el movimiento no es deseable en absoluto
    * y no debe ser considerado una respuesta válida.
  */
  def movimentoMasEfectivoContra(guerrero: Guerrero)(criterio: Criterio): Option[Movimiento] = {
    if ((movimientos map criterio.simular(this, guerrero)).forall( _ < 0 )) None // ¯\_(ツ)_/¯
    else Some(movimientos.maxBy(criterio.simular(this, guerrero)))
  }

  /**
    * Cuando un guerrero pelea un round, realiza un movimiento (previamente elegido) contra el oponente.
    * Inmediatamente después de sufrir el efecto del movimiento, el oponente realiza un contraataque.
    * El sistema debe asumir que el oponente siempre realiza el ataque que lo deje con la mayor ventaja
    * (o al menos, con la menor desventaja) sobre los puntos de ki.
    * Al finalizar el round, el usuario debe poder tener acceso al nuevo estado del atacante y el defensor.
    */
  // siempre existe un movimiento que sea la menor desventaja
  def pelearRound(movimiento: Movimiento)(guerrero: Guerrero): (Guerrero, Guerrero) = {
    val (yo, elOtro) = this.atacar(guerrero, movimiento)
    val (elOtro2, yo2) = elOtro.atacar(yo, elOtro.movimentoMasEfectivoContra(yo)(MenorDesventaja).get)
    (yo2, elOtro2)
  }

  /**
    * Para cada round solicitado, el sistema debe elegir el movimiento más efectivo de acuerdo al criterio
    * recibido para realizar contra el oponente.
    * Luego debe simular la pelea de dicho round utilizando el movimiento elegido
    * y seleccionar el movimiento para el siguiente round basándose en el resultado de este.
    *
    * Si el guerrero no encuentra un movimiento satisfactorio para cada round pedido,
    * NO DEBE retornarse un plan más corto.
    *
    * BONUS: Hacerlo sin usar recursividad ni asignación destructiva!
    */
  def planDeAtaqueContra(guerrero: Guerrero, rounds: Int)(criterio: Criterio): PlanDeAtaque = {

    val emptyPlan: PlanDeAtaque = Option(Seq[Movimiento]())
    val seed: ((Guerrero, Guerrero), PlanDeAtaque) = ((this, guerrero), emptyPlan)

    val (_, plan) = (1 to rounds).foldLeft(seed) { (res, _) =>

      val ((atacante, atacado), plan) = res
      val movimiento = atacante.movimentoMasEfectivoContra(atacado)(criterio)

      movimiento.fold(((atacante, atacado), emptyPlan)) { mov =>
        val modifiedPlan = plan map { seq => seq :+ mov }
        val nextState = atacante.pelearRound(mov)(atacado)
        (nextState, modifiedPlan)
      }

    }

    plan
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
  def pelearContra(guerrero: Guerrero)(planDeAtaque: PlanDeAtaque): Option[ResultadoPelea] = {
    val seed: ResultadoPelea = SiguenPeleando(this, guerrero)
    planDeAtaque.map { _.foldLeft(seed) { (s, m) => s.map(pelearRound(m)) } }
  }
}

trait Fusionable

case class Caracteristicas(nombre: String, inventario: List[Item], movimientos: List[Movimiento], energiaMax: Int, energia: Int)

case class Humano(caracteristicas: Caracteristicas) extends Guerrero with Fusionable {
  def copiarConEnergia(energia: Int): Humano = copy(caracteristicas copy (energia = energia))
  def copiarConItems(items: List[Item]): Humano = copy(caracteristicas copy (inventario = items))
  def copiarConMovimientos(movimientos: List[Movimiento]): Humano = copy(caracteristicas copy (movimientos = movimientos))
}

case class Saiyajin(caracteristicas: Caracteristicas, cola: Boolean = true, nivelSaiyajin: Int = 0, estadoMono: Boolean = false)
  extends Guerrero with Fusionable {
  def copiarConEnergia(energia: Int): Saiyajin = copy(caracteristicas copy (energia = energia))
  def copiarConItems(items: List[Item]): Saiyajin = copy(caracteristicas copy (inventario = items))
  def copiarConMovimientos(movimientos: List[Movimiento]): Saiyajin = copy(caracteristicas copy (movimientos = movimientos))
}

case class Androide(caracteristicas: Caracteristicas) extends Guerrero {
  def copiarConEnergia(energia: Int): Androide = copy(caracteristicas copy (energia = energia))
  def copiarConItems(items: List[Item]): Androide = copy(caracteristicas copy (inventario = items))
  def copiarConMovimientos(movimientos: List[Movimiento]): Androide = copy(caracteristicas copy (movimientos = movimientos))
}

case class Namekusein(caracteristicas: Caracteristicas) extends Guerrero with Fusionable {
  def copiarConEnergia(energia: Int): Namekusein = copy(caracteristicas copy (energia = energia))
  def copiarConItems(items: List[Item]): Namekusein = copy(caracteristicas copy (inventario = items))
  def copiarConMovimientos(movimientos: List[Movimiento]): Namekusein = copy(caracteristicas copy (movimientos = movimientos))
}

case class Monstruo(caracteristicas: Caracteristicas, formaDeDigerir: ((Guerrero, Guerrero) => Guerrero)) extends Guerrero {
  def copiarConEnergia(energia: Int): Monstruo = copy(caracteristicas copy (energia = energia))
  def copiarConItems(items: List[Item]): Monstruo = copy(caracteristicas copy (inventario = items))
  def copiarConMovimientos(movimientos: List[Movimiento]): Monstruo = copy(caracteristicas copy (movimientos = movimientos))
}

case class Fusionado(caracteristicas: Caracteristicas) extends Guerrero {
  def copiarConEnergia(energia: Int): Fusionado = copy(caracteristicas copy (energia = energia))
  def copiarConItems(items: List[Item]): Fusionado = copy(caracteristicas copy (inventario = items))
  def copiarConMovimientos(movimientos: List[Movimiento]): Fusionado = copy(caracteristicas copy (movimientos = movimientos))
}