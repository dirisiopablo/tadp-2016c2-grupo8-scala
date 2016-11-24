import Guerrero._
import Item._
import Movimiento._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MovimientoSuite extends FunSuite {

  trait MovimientoTest {
    val listaItemsVegeta = List(SemillaDelErmitanio, FotoDeLaLuna)
    val listaMovimientosVegeta = List(Cargar, ConvertirseEnMono, UsarItem(SemillaDelErmitanio))
    val caracteristicasVegeta = Caracteristicas("Vegeta", listaItemsVegeta, listaMovimientosVegeta, 8000, 6400)
    val vegeta = Guerrero(caracteristicasVegeta ,Saiyajin(cola = true, nivelSaiyajin = 0, estadoMono = false, inconsciente = true))

    val listaItemsGoku = List(SemillaDelErmitanio)
    val listaMovimientosGoku = List(FusionarseCon(vegeta), ConvertirseEnSuperSaiyajin, UsarItem(SemillaDelErmitanio))
    val caracteristicasGoku = Caracteristicas("Goku", listaItemsGoku, listaMovimientosGoku, 9999, 6000)
    val goku = Guerrero(caracteristicasGoku ,Saiyajin(cola = false, nivelSaiyajin = 0, estadoMono = false, inconsciente = true))

    val listaItemsYamcha = List()
    val listaMovimientosYamcha = List(DejarseFajar)
    val caracteristicasYamcha = Caracteristicas("Yamcha", listaItemsYamcha, listaMovimientosYamcha, 1, 1)
    val yamcha = Guerrero(caracteristicasYamcha, Humano(inconsciente = true))

    val listaItemsPiccolo = List(EsferaDeUnaEstrella, EsferaDeDosEstrellas, EsferaDeTresEstrellas,
      EsferaDeCuatroEstrellas, EsferaDeCincoEstrellas, EsferaDeSeisEstrellas, EsferaDeSieteEstrellas)
    val esMagique = {(g1:Guerrero, g2:Guerrero) => (g1, g2.copiarConEnergia(2))}
    val listaMovimientosPiccolo = List(Magia(esMagique))
    val caracteristicasPiccolo = Caracteristicas("Piccolo", listaItemsPiccolo, listaMovimientosPiccolo, 1700, 1700)
    val piccolo = Guerrero(caracteristicasPiccolo, Namekusein(inconsciente = true))

    val listaItemsBuu = List()
    val listaMovimientosBuu = List(ComerseAlOponente)
    val caracteristicasBuu = Caracteristicas("Buu", listaItemsBuu, listaMovimientosBuu, 3500, 3500)
    val formaDigerirBuu = {(g: Guerrero, g2: Guerrero) => g.copiarConMovimientos(listaMovimientosBuu ++ g2.movimientos)}
    val buu = Guerrero(caracteristicasBuu, Monstruo(formaDigerirBuu, inconsciente = true))
  }

  test("Usar Semilla del Ermitaño") {
    new MovimientoTest {
      val(v, _) = vegeta.atacar(goku, UsarItem(SemillaDelErmitanio))
      assert(v.energia  === vegeta.energiaMax)
    }
  }

  test("Convertirse en Mono") {
    new MovimientoTest {
      val(v, _) = vegeta.atacar(goku, ConvertirseEnMono)
      v.tipo match {
        case Saiyajin(true, 0, true, _) if v.energia === vegeta.energiaMax * 3 && v.energia === v.energiaMax => succeed
        case _ => fail
      }
    }
  }

  test("Convertirse en Super Saiyajin") {
    new MovimientoTest {
      val(g, v) = goku.atacar(vegeta, ConvertirseEnSuperSaiyajin)
      g.tipo match {
        case Saiyajin(false, 1, false, _) if g.energiaMax === goku.energiaMax * 5 && g.energia === goku.energia => succeed
        case _ => fail
      }
    }
  }

  test("Convertirse en Super Saiyajin 2") {
    new MovimientoTest {
      val(gssj1, _) = goku.atacar(vegeta, ConvertirseEnSuperSaiyajin)
      val(gssj1curado, _) = gssj1.atacar(vegeta, UsarItem(SemillaDelErmitanio))
      val(gssj2, _) = gssj1curado.atacar(vegeta, ConvertirseEnSuperSaiyajin)

      gssj2.tipo match {
        case Saiyajin(false, 2, false, _)
          if gssj2.energiaMax === goku.energiaMax * 25 && gssj2.energia === gssj1curado.energia => succeed
        case _ => fail
      }

    }
  }

  test("Cargar normal") {
    new MovimientoTest {
      val (g, _) = goku.atacar(vegeta, Cargar)
      assert(g.energia === goku.energia + 100)
    }
  }

  test("Cargar ssj") {
    new MovimientoTest {
      val(gssj, _) = goku.atacar(vegeta, ConvertirseEnSuperSaiyajin)
      val(g2, _) = gssj.atacar(vegeta, Cargar)
      assert(g2.energia === goku.energia + 150)
    }
  }

  test("Cargar ssj2") {
    new MovimientoTest {
      val(gssj1, _) = goku.atacar(vegeta, ConvertirseEnSuperSaiyajin)
      val(gssj1curado, _) = gssj1.atacar(vegeta, UsarItem(SemillaDelErmitanio))
      val(gssj2, _) = gssj1curado.atacar(vegeta, ConvertirseEnSuperSaiyajin)
      val(g, _) = gssj2.atacar(vegeta, Cargar)
      assert(g.energia === gssj1curado.energia + 300)
    }
  }

  test("Monstruo come oponente") {
    new MovimientoTest {
      val (b, _) = buu.atacar(yamcha, ComerseAlOponente)
      assert(b.movimientos === buu.movimientos ++ yamcha.movimientos)
    }
  }

  test("Yamcha come oponente y pasa verguenza") {
    new MovimientoTest {
      val (y, v) = yamcha.atacar(vegeta, ComerseAlOponente)
      assert((y, v) === (yamcha, vegeta))
    }
  }

  test("Yamcha se deja fajar porque es un cono") {
    new MovimientoTest {
      val (y, v) = yamcha.atacar(vegeta, DejarseFajar)
      assert((y, v) === (yamcha, vegeta))
    }
  }

  test("Goku se fusiona con vegeta") {
    new MovimientoTest {
      val (vg, b) = goku.atacar(buu, FusionarseCon(vegeta))
      assert(
        vg.movimientos === goku.movimientos ++ vegeta.movimientos
        && vg.energia === goku.energia + vegeta.energia
        && vg.energiaMax === goku.energiaMax + vegeta.energiaMax
      )
    }
  }

  test("Magia") {
    new MovimientoTest {
      val (p, b) = piccolo.atacar(buu, Magia(esMagique))
      assert(p === piccolo && b.energia === 2)
    }
  }

}