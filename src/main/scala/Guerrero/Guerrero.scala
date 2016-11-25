package Guerrero

import Item._
import Movimiento.{DejarseFajar, Genkidama, Movimiento}
import ResultadoPelea.{ResultadoPelea, SiguenPeleando}
import Criterio.{Criterio, MenorDesventaja}

case class Guerrero(caracteristicas: Caracteristicas, tipo: Tipo, energiaAcumulada: Int = 0) {
  type PlanDeAtaque = Option[Seq[Movimiento]]

  def movimientos = caracteristicas.movimientos

  def nombre = caracteristicas.nombre

  def energia = caracteristicas.energia
  def energiaMax = caracteristicas.energiaMax

  def itemList = caracteristicas.inventario
  def tieneItem(item: Item) = itemList contains item

  def copiarConEnergia(energia: Int): Guerrero = copy(caracteristicas copy (energia = energia))
  def copiarConItems(items: List[Item]): Guerrero = copy(caracteristicas copy (inventario = items))
  def copiarConMovimientos(movimientos: List[Movimiento]): Guerrero = copy(caracteristicas copy (movimientos = movimientos))

  def eliminarItem(i: Item): Guerrero = copy(caracteristicas copy (inventario = itemList diff List(i)))

  def atacar(guerrero: Guerrero, movimiento: Movimiento): (Guerrero, Guerrero) = {
    if ((movimientos contains Genkidama) && movimiento == DejarseFajar)
      movimiento(copy(energiaAcumulada = this.energiaAcumulada + 1), guerrero)
    else if(movimiento == Genkidama) {
      val (g1, g2) = movimiento(this, guerrero)
      (g1.copy(energiaAcumulada = 0), g2) // para borrar la energia DESPUES de tirar la genkidama
    }
    else
      movimiento(copy(energiaAcumulada = 0), guerrero)
  }

  /**
    * Si el resultado del criterio es menor a 0 significa que el movimiento no es deseable en absoluto
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
    val (yo, elOtro) = this.atacar(guerrero, movimiento) // ataque
    val (elOtro2, yo2) = elOtro.atacar(yo, elOtro.movimentoMasEfectivoContra(yo)(MenorDesventaja).get) // contraataque
    (yo2, elOtro2) // estado final
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
    planDeAtaque.map { _.foldLeft(seed) { (s, m) => s map { g => g._1.pelearRound(m)(g._2) } } }
  }
}

sealed trait Tipo
trait Fusionable
trait Inconscientable {
  val inconsciente: Boolean
  def copiarInconsciente: Tipo
}

case class Caracteristicas(nombre: String, inventario: List[Item], movimientos: List[Movimiento], energiaMax: Int, energia: Int)

case class Humano(inconsciente: Boolean) extends Tipo with Fusionable with Inconscientable {
  def copiarInconsciente: Humano = copy(inconsciente = true)
}

case class Saiyajin(cola: Boolean = true, nivelSaiyajin: Int = 0, estadoMono: Boolean = false, inconsciente: Boolean)
  extends Tipo with Fusionable with Inconscientable {
  def copiarInconsciente: Saiyajin = copy(inconsciente = true)
}

case class Androide() extends Tipo

case class Namekusein(inconsciente: Boolean) extends Tipo with Fusionable with Inconscientable {
  def copiarInconsciente: Namekusein = copy(inconsciente = true)
}

case class Monstruo(formaDeDigerir: ((Guerrero, Guerrero) => Guerrero), inconsciente: Boolean) extends Tipo with Inconscientable {
  def copiarInconsciente: Monstruo = copy(inconsciente = true)
}

case class Fusionado(inconsciente: Boolean) extends Tipo with Inconscientable {
  def copiarInconsciente: Fusionado = copy(inconsciente = true)
}