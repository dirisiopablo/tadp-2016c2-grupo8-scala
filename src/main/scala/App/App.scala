package App

object App {

  type PlanDeAtaque = Seq[Movimiento]
  type ResultadoPelea = (Guerrero, Guerrero)

  /**
    Los criterios pueden ser de lo más variados:
    - usar el movimiento que más daño le haga al enemigo
    - usar el movimiento que deje a su enemigo con la mayor cantidad de ki
    - usar los movimientos que les hagan perder la menor cantidad de ítems
    - usar cualquier movimiento que no lo mate.

    Para simplificar la decisión, se pide modelar los criterios como cuantificadores del resultado de realizar el movimiento;
    es decir, un criterio debe procesar el estado del atacante y el defensor luego de realizar el movimiento
    y producir un número que represente qué tan “deseado” es dicho resultado.
    Cuanto más grande sea el número, más favorecido será el movimiento analizado.
    Si el resultado del criterio es igual o menor a 0 significa que el movimiento no es deseable en absoluto
    y no debe ser considerado una respuesta válida.

    Es importante tener en cuenta que el guerrero podría no disponer de ningún movimiento que satisfaga el criterio,
    lo cual debe ser manejado de forma acorde.
    En caso de que el criterio produzca el mismo valor para más de un movimiento, se puede elegir cualquier de ellos.
    */
  trait Criterio {
    def simular(atacante: Guerrero, atacado: Guerrero)(movimiento: Movimiento): (Movimiento, Int)
  }

  // mayor daño al otro
  case object MayorDanio extends Criterio {
    override def simular(atacante: Guerrero, atacado: Guerrero)(movimiento: Movimiento): (Movimiento, Int) = ???
  }

  // menor daño al otro
  case object MenorDanio extends Criterio {
    override def simular(atacante: Guerrero, atacado: Guerrero)(movimiento: Movimiento): (Movimiento, Int) = ???
  }

  // menor diferencia de ki entre los 2
  case object MenorDesventaja extends Criterio {
    override def simular(atacante: Guerrero, atacado: Guerrero)(movimiento: Movimiento): (Movimiento, Int) = ???
  }

  // el que gaste menos items del atacante
  case object GastarMenosItems extends Criterio {
    override def simular(atacante: Guerrero, atacado: Guerrero)(movimiento: Movimiento): (Movimiento, Int) = ???
  }

  // cualquier cosa que no deje al atacante en 0
  case object QueNoMeMate extends Criterio {
    override def simular(atacante: Guerrero, atacado: Guerrero)(movimiento: Movimiento): (Movimiento, Int) = ???
  }

  trait Movimiento
  trait Ataque extends Movimiento
  trait AtaqueFisico extends Ataque
  trait AtaqueEnergia extends Ataque

  case class Item()

  trait Guerrero {
    val nombre: String = ???
    val inventario: List[Item] = ???
    val movimientos: List[Movimiento] = ???
    val ki: Int = ???

    def atacar(guerrero: Guerrero, movimiento: Movimiento): ResultadoPelea = ???
      // movimiento(this) -- overrideando el apply
      // si necesita un target, que sea movimiento(this)(target)

    def movimentoMasEfectivoContra(guerrero: Guerrero)(criterio: Criterio): Movimiento =
      movimientos.map{criterio.simular(this, guerrero)}.maxBy(_._2)._1

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
      def iterate(s: state): state = {
        val yo = s._1._1
        val elOtro = s._1._2
        val mov = yo.movimentoMasEfectivoContra(elOtro)(criterio)
        (yo.pelearRound(mov)(elOtro), s._2 :+ mov)
      }

      val seed = ((this, guerrero), Seq[Movimiento]())
      (1 to rounds).foldLeft(seed) { (a, _) => iterate(a) }._2
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
    def pelearContra(guerrero: Guerrero)(planDeAtaque: PlanDeAtaque): ResultadoPelea = {
      val seed: ResultadoPelea = (this, guerrero)
      planDeAtaque.foldLeft(seed) { (res: ResultadoPelea, m: Movimiento) =>
        if(res._1.ki <= 0 || res._2.ki <= 0) return res
        res._1.pelearRound(m)(res._2)
      }
    }

  }

  case class Humano() extends Guerrero
  case class Saiyajin(cola: Boolean) extends Guerrero
  case class Androide(bateria: Int) extends Guerrero
  case class Namekusein() extends Guerrero
  case class Monstruo() extends Guerrero

}

