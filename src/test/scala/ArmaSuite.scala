import Guerrero._
import Item._
import Movimiento._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ArmaSuite extends FunSuite {

  trait MovimientoTest {
    val listaItemsVegeta = List(SemillaDelErmitanio, FotoDeLaLuna)
    val listaMovimientosVegeta = List(Cargar, ConvertirseEnMono, UsarItem(SemillaDelErmitanio))
    val caracteristicasVegeta = Caracteristicas("Vegeta", listaItemsVegeta, listaMovimientosVegeta, 8000, 6400)
    val vegeta = Saiyajin(caracteristicasVegeta, cola = true, nivelSaiyajin = 0, estadoMono = false)

    val listaItemsGoku = List(SemillaDelErmitanio)
    val listaMovimientosGoku = List(FusionarseCon(vegeta), ConvertirseEnSuperSaiyajin, UsarItem(SemillaDelErmitanio))
    val caracteristicasGoku = Caracteristicas("Goku", listaItemsGoku, listaMovimientosGoku, 9999, 6000)
    val goku = Saiyajin(caracteristicasGoku, cola = false, nivelSaiyajin = 0, estadoMono = false)

    val listaItemsYamcha = List()
    val listaMovimientosYamcha = List(DejarseFajar)
    val caracteristicasYamcha = Caracteristicas("Yamcha", listaItemsYamcha, listaMovimientosYamcha, 1, 1)
    val yamcha = Humano(caracteristicasYamcha)

    val listaItemsPiccolo = List(EsferaDeUnaEstrella, EsferaDeDosEstrellas, EsferaDeTresEstrellas,
      EsferaDeCuatroEstrellas, EsferaDeCincoEstrellas, EsferaDeSeisEstrellas, EsferaDeSieteEstrellas)
    val listaMovimientosPiccolo = List(Magia)
    val caracteristicasPiccolo = Caracteristicas("Piccolo", listaItemsPiccolo, listaMovimientosPiccolo, 1700, 1700)
    val piccolo = Namekusein(caracteristicasPiccolo)

    val listaItemsBuu = List()
    val listaMovimientosBuu = List(ComerseAlOponente)
    val caracteristicasBuu = Caracteristicas("Buu", listaItemsBuu, listaMovimientosBuu, 3500, 3500)
    val formaDigerirBuu = {(g: Guerrero, g2: Guerrero) => g.copiarConMovimientos(listaMovimientosBuu ++ g2.movimientos)}
    val buu = Monstruo(caracteristicasBuu, formaDigerirBuu)
  }

  test("Armas") {
    assert(1 === 2)
  }

}