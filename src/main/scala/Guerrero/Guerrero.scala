package Guerrero

import Item._
import Movimiento.Movimiento
import Criterio.{Criterio, MenorDesventaja}

trait Guerrero {

  type PlanDeAtaque = Seq[Movimiento]
  type ResultadoPelea = (Guerrero, Guerrero)

  val nombre: String
  val inventario: List[Item]
  val movimientos: List[Movimiento]

  def atacar(guerrero: Guerrero, movimiento: Movimiento): ResultadoPelea =
    movimiento.aplicar(this, guerrero)

  def movimentoMasEfectivoContra(guerrero: Guerrero)(criterio: Criterio): Movimiento =
    movimientos.map{criterio.simular(this, guerrero)}.maxBy(_._2)._1
  //FIXME: preguntar dónde poner la exception de ningún movimiento cumple. Por ejemplo 'cualquier cosa que no deje al ejecutante en 0'

  /**
  Cuando un guerrero pelea un round, realiza un movimiento (previamente elegido) contra el oponente.
      Inmediatamente después de sufrir el efecto del movimiento, el oponente realiza un contraataque.
      El sistema debe asumir que el oponente siempre realiza el ataque que lo deje con la mayor ventaja
      (o al menos, con la menor desventaja) sobre los puntos de ki.
      Al finalizar el round, el usuario debe poder tener acceso al nuevo estado del atacante y el defensor.
    */
  def pelearRound(movimiento: Movimiento)(guerrero: Guerrero): ResultadoPelea = {
    val (yo, elOtro) = this.atacar(guerrero, movimiento)
    elOtro.atacar(yo, elOtro.movimentoMasEfectivoContra(yo)(MenorDesventaja))
  }

  /**
  Para cada round solicitado, el sistema debe elegir el movimiento más efectivo de acuerdo al criterio recibido
      para realizar contra el oponente.
      Luego debe simular la pelea de dicho round utilizando el movimiento elegido
      y seleccionar el movimiento para el siguiente round basándose en el resultado de este.
      Si el guerrero no encuentra un movimiento satisfactorio para cada round pedido, NO DEBE retornarse un plan más corto.
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
  Uno de los dos peleadores ganó:
        - Esto ocurre solamente cuando el oponente muere.
          Queremos saber quién de los dos fué y en qué estado quedó.
          El estado del perdedor no importa.

      Ambos siguen peleando:
        - La pelea no tiene un ganador definido, pero queremos saber en qué estado están.
          Para ganar, no es necesario pelear todos los rounds previstos en el plan.
          Si uno de los peleadores muere durante un round, ese round se termina de pelear, se declara al ganador
          y ya NO DEBEN PELEARSE LOS ROUNDS SIGUIENTES;
          es decir, el estado del ganador es el que le quede en el round que ganó.

      Si ambos peleadores mueren en el mismo round se considera que el ganador es el receptor del mensaje
      (después de todo, cumplió el objetivo de su ataque...).

      BONUS: Hacerlo sin usar recursividad ni asignación destructiva!
    */
  //def pelearContra(guerrero: Guerrero)(planDeAtaque: PlanDeAtaque): ResultadoPelea = {
    // si el bonus era para el punto 3, cambiar el fold por una recursion con pattern matching
    // para sacar ese return de mierda
    //val seed: ResultadoPelea = (this, guerrero)
    //planDeAtaque.foldLeft(seed) { (res: ResultadoPelea, m: Movimiento) =>
     // if(res._1.ki <= 0 || res._2.ki <= 0) return res
      //res._1.pelearRound(m)(res._2)
    //}
  //}

}

trait Fusionable

case class Humano(nombre: String, inventario: List[Item], movimientos: List[Movimiento], kiMax: Int, ki: Int) extends Guerrero with Fusionable {}
case class Saiyajin(nombre: String, inventario: List[Item], movimientos: List[Movimiento], kiMax: Int, ki: Int, cola: Boolean = true, nivelSaiyajin: Int = 0, estadoMono: Boolean = false) extends Guerrero with Fusionable {
  //def intentarTransformarse() = {
    // if (kiMax / 2 <= ki) throw new Exception("El saiyajin no tiene el ki suficiente para transformarse.")
    //else if (estadoMono) throw new Exception("El saiyajin está convertido en mono.")
    //    else {
  //  kiMax = kiMax * 5
    //}
  //}

//  def convertirseEnMono() = {
//    if (inventario.contains("foto luna") && cola)
//  }
}

case class Androide(nombre: String, inventario: List[Item], movimientos: List[Movimiento], bateriaMax: Int, bateria: Int) extends Guerrero
case class Namekusein(nombre: String, inventario: List[Item], movimientos: List[Movimiento], kiMax: Int, ki: Int) extends Guerrero with Fusionable
case class Monstruo(nombre: String, inventario: List[Item], movimientos: List[Movimiento], kiMax: Int, ki: Int, formaDeDigerir: (Guerrero => Guerrero)) extends Guerrero
case class Fusionado(nombre: String, inventario: List[Item], movimientos: List[Movimiento], kiMax: Int, ki: Int) extends Guerrero