import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Guerrero._
import Movimiento._

@RunWith(classOf[JUnitRunner])
class AtaqueSuite extends FunSuite {

  trait AtaqueTest {
    val listaItemsGoku = List()
    val listaMovimientosGoku = List(MuchosGolpes, Kamehameha)
    val caracteristicasGoku = Caracteristicas("Goku", listaItemsGoku, listaMovimientosGoku, 9999, 9000)
    val goku = Saiyajin(caracteristicasGoku, cola = false, nivelSaiyajin = 0, estadoMono = false, inconsciente = false)

    val listaItemsVegeta = List()
    val listaMovimientosVegeta = List(MuchosGolpes, FinalFlash)
    val caracteristicasVegeta = Caracteristicas("Vegeta", listaItemsVegeta, listaMovimientosVegeta, 8000, 7500)
    val vegeta = Saiyajin(caracteristicasVegeta, cola = false, nivelSaiyajin = 0, estadoMono = false, inconsciente = false)

    val listaItemsKrilin = List()
    val listaMovimientosKrilin = List(OndaVitalTio)
    // Si, ya arranca muerto el manco
    val caracteristicasKrilin = Caracteristicas("Krilin", listaItemsKrilin, listaMovimientosKrilin, 100, 0)
    val krilin = Humano(caracteristicasKrilin, inconsciente = false)

    val listaItems18 = List()
    val listaMovimientos18 = List(Explotar)
    val caracteristicas18 = Caracteristicas("18", listaItems18, listaMovimientos18, 2500, 2500)
    val a18 = Androide(caracteristicas18)

    val listaItemsYamcha = List()
    val listaMovimientosYamcha = List(MuchosGolpes, OndaVitalTio)
    val caracteristicasYamcha = Caracteristicas("Yamcha", listaItemsYamcha, listaMovimientosYamcha, 1, 1)
    val yamcha = Humano(caracteristicasYamcha, inconsciente = false)

    val listaItemsPiccolo = List()
    val listaMovimientosPiccolo = List(MuchosGolpes)
    val caracteristicasPiccolo = Caracteristicas("Piccolo", listaItemsPiccolo, listaMovimientosPiccolo, 1700, 1700)
    val piccolo = Namekusein(caracteristicasPiccolo, inconsciente = false)

    val piccoloInconsciente = Namekusein(caracteristicasPiccolo, inconsciente = true)

    val listaItemsBuu = List()
    val listaMovimientosBuu = List(Explotar)
    val caracteristicasBuu = Caracteristicas("Buu", listaItemsBuu, listaMovimientosBuu, 3500, 3500)
    val formaDigerirBuu = {(g: Guerrero, g2: Guerrero) => g}
    val buu = Monstruo(caracteristicasBuu, formaDigerirBuu, inconsciente = false)
  }

  test("Muchos golpes con mas energia") {
    new AtaqueTest {
      // vegeta es atacado y 20 porque arranca con menos energia que goku
      val (_, v) = goku.atacar(vegeta, MuchosGolpes)
      assert(v.energia === vegeta.energia - 20)
    }
  }

  test("Muchos golpes con menos energia") {
    new AtaqueTest {
      // vegeta ataca y pierde 20 porque arranca con menos energia que goku
      val (v2, _) = vegeta.atacar(goku, MuchosGolpes)
      assert(v2.energia === vegeta.energia - 20)
    }
  }

  test("Muchos golpes a un androide") {
    new AtaqueTest {
      // yamcha se lastima los deditos porque es un cono
      val(yam, _) = yamcha.atacar(a18, MuchosGolpes)
      assert(yam.energia === yamcha.energia - 10)
    }
  }

  test("Mounstro explosivo") {
    new AtaqueTest {
      val(b, g) = buu.atacar(goku, Explotar)
      assert(b.energia === 0 && g.energia === goku.energia - buu.energia * 2)
    }
  }

  test("Androide explosivo") {
    new AtaqueTest {
      val(a, g) = a18.atacar(goku, Explotar)
      assert(a.energia === 0 && g.energia === goku.energia - a18.energia * 3)
    }
  }

  test("Piccolo no muere con explosiones") {
    new AtaqueTest {
      val(a, p) = a18.atacar(piccolo, Explotar)
      assert(a.energia === 0 && p.energia === 1)
    }
  }

  test("Yamcha no puede explotar") {
    new AtaqueTest {
      val(y, p) = yamcha.atacar(piccolo, Explotar)
      assert((y, p) === (yamcha, piccolo))
    }
  }

  test("Androide absorbe ataque de energia") {
    new AtaqueTest {
      val energiaFinalRequerida = a18.energia + Kamehameha.kiRequerido
      val(g, a) = goku.atacar(a18, Kamehameha)
      assert(energiaFinalRequerida == a.energia)
    }
  }

  test("Monstruo recibe kirequerido/2 de daño con ataque de energia") {
    new AtaqueTest {
      val energiaFinalRequerida = buu.energia - FinalFlash.kiRequerido / 2
      val(v, b) = vegeta.atacar(buu, FinalFlash)
      assert(energiaFinalRequerida == b.energia)
    }
  }

  test("Ataque de energia funciona") {
    new AtaqueTest {
      val energiaFinalRequerida = vegeta.energia - Kamehameha.kiRequerido * 2
      val(g, v) = goku.atacar(vegeta, Kamehameha)
      assert(energiaFinalRequerida == v.energia)
    }
  }

  test("Genkidama pega 10^rounds que se dejo fajar") {
    new AtaqueTest {
      assert(1 === 2)
    }
  }

  test("Guerrero inconsciente no ataca") {
    new AtaqueTest {
      val (pi, v) = piccoloInconsciente.atacar(vegeta, MuchosGolpes)
      assert((pi, v) === (piccoloInconsciente, vegeta))
    }
  }

  test("Guerrero muerto no ataca") {
    new AtaqueTest {
      val(k, b) = krilin.atacar(buu, OndaVitalTio)
      assert((k, b) === (krilin, buu))
    }
  }

}