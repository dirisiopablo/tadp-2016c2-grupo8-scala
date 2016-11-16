import Guerrero._
import Item._
import Movimiento._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ArmaSuite extends FunSuite {

  trait ArmaTest {
    val caracteristicasVegeta = Caracteristicas("Vegeta", List(), List(), 8000, 6400)
    val vegeta = Saiyajin(caracteristicasVegeta, cola = true, nivelSaiyajin = 0, estadoMono = false)
    val vegetaMono = Saiyajin(caracteristicasVegeta, cola = true, nivelSaiyajin = 0, estadoMono = true)

    val listaItemsTrunks = List(BraveSword)
    val listaMovimientosTrunks = List(UsarItem(BraveSword))
    val caracteristicasTrunks = Caracteristicas("Trunks", listaItemsTrunks, listaMovimientosTrunks, 5000, 1500)
    val trunks = Saiyajin(caracteristicasTrunks, cola = false, nivelSaiyajin = 0, estadoMono = false)

    val caracteristicasYamcha = Caracteristicas("Yamcha", List(), List(), 700, 700)
    val yamcha = Humano(caracteristicasYamcha)

    val listaItemsPiccolo = List(EsferaDeUnaEstrella, EsferaDeDosEstrellas, EsferaDeTresEstrellas,
      EsferaDeCuatroEstrellas, EsferaDeCincoEstrellas, EsferaDeSeisEstrellas, EsferaDeSieteEstrellas)
    val esMagique = {(g1:Guerrero, g2:Guerrero) => (g1, g2.copiarConEnergia(2))}
    val listaMovimientosPiccolo = List(Magia(esMagique))
    val caracteristicasPiccolo = Caracteristicas("Piccolo", listaItemsPiccolo, listaMovimientosPiccolo, 1700, 1700)
    val piccolo = Namekusein(caracteristicasPiccolo)

    val listaItemsLaunch = List(Chumbo, Municion, Municion, Municion) // TODO agregar otro tipo de municion
    val listaMovimientosLaunch = List(UsarItem(Chumbo))
    val caracteristicasLaunch = Caracteristicas("Launch", listaItemsLaunch, listaMovimientosLaunch, 50, 50)
    val launch = Humano(caracteristicasLaunch)
  }

  test("El arma no se consume") {
    new ArmaTest {
      val (t, _) = trunks.atacar(vegeta, UsarItem(BraveSword))
      assert(t.itemList.length !== 0)
    }
  }

  test("Arma filosa corta cola de saiyajin") {
    new ArmaTest {
      val (_, v) = trunks.atacar(vegeta, UsarItem(BraveSword))
      v match {
        case Saiyajin(_, false, _, _) => succeed
        case _ => fail
      }
    }
  }

  test("Arma filosa deja saiyajin con cola en 1") {
    new ArmaTest {
      val (_, v) = trunks.atacar(vegeta, UsarItem(BraveSword))
      assert(v.energia === 1)
    }
  }

  test("Arma filosa deja saiyajin mono inconsciente") {
    new ArmaTest {
      assert(1 === 2)
    }
  }

  test("Arma filosa baja 1 energia por cada 100 del atacante") {
    new ArmaTest {
      val (_, y) = trunks.atacar(yamcha, UsarItem(BraveSword))
      assert(y.energia === yamcha.energia - trunks.energia / 100)
    }
  }

  test("Arma de fuego no hace nada sin municion adecuada") {
    new ArmaTest {
      assert(1 === 2)
    }
  }

  test("Arma de fuego no hace nada si no sos humano o namek inconsciente") {
    new ArmaTest {
      assert(1 === 2)
    }
  }

  test("Arma de fuego gasta municion adecuada") {
    new ArmaTest {
      assert(1 === 2)
    }
  }

  test("Arma de fuego no hace nada contra namek no inconsciente") {
    new ArmaTest {
      assert(1 === 2)
    }
  }

  test("Arma de fuego pega 10 a namek inconsciente") {
    new ArmaTest {
      assert(1 === 2)
    }
  }

  test("Arma de fuego pega 20 a humano") {
    new ArmaTest {
      assert(1 === 2)
    }
  }

  test("Arma roma deja inconsciente a androide con menos de 300 energia") {
    new ArmaTest {
      assert(1 === 2)
    }
  }

  test("Arma roma no hace nada contra otra cosa que no sea androide con menos de 300 de energia") {
    new ArmaTest {
      assert(1 === 2)
    }
  }

}