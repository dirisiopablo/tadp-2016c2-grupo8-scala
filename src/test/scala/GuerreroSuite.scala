import Criterio.{MayorDanio, MenorDanio}
import Guerrero._
import Item._
import Movimiento._
import ResultadoPelea.SiguenPeleando
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GuerreroSuite extends FunSuite {

  trait GuerreroTest {
    val listaItemsGoku = List()
    val listaMovimientosGoku = List(MuchosGolpes)
    val caracteristicasGoku = Caracteristicas("Goku", listaItemsGoku, listaMovimientosGoku, 9999, 9000)
    val goku = Guerrero(caracteristicasGoku, Saiyajin(cola = false, 0, estadoMono = false, inconsciente = false))

    val listaItemsVegeta = List(SemillaDelErmitanio)
    val listaMovimientosVegeta = List[Movimiento](Cargar, UsarItem(SemillaDelErmitanio), MuchosGolpes)
    val caracteristicasVegeta = Caracteristicas("Vegeta", listaItemsVegeta, listaMovimientosVegeta, 8000, 1000)
    val vegeta = Guerrero(caracteristicasVegeta, Saiyajin(cola = false, 0, estadoMono = false, inconsciente = false))

    val listaItemsKrilin = List()
    val listaMovimientosKrilin = List(Kamehameha)
    val caracteristicasKrilin = Caracteristicas("Krilin", listaItemsKrilin, listaMovimientosKrilin, 5000, 5000)
    val krilin = Guerrero(caracteristicasKrilin, Humano(inconsciente = false))
  }

  test("Pelear Round") {
    new GuerreroTest {
      val (g, v) = goku.pelearRound(MuchosGolpes)(vegeta)
      // vegeta deberia usar la semilla (contrataca con el movimiento que lo deje en menor desventaja)
      assert(g.energia === 9000 && v.energia === 8000 && v.itemList.length === 0)
    }
  }

  test("Pelear Contra") {
    new GuerreroTest {
      val planDeAtaque = Some(List(MuchosGolpes, MuchosGolpes, MuchosGolpes))
      val res: Option[ResultadoPelea.ResultadoPelea] = goku.pelearContra(krilin)(planDeAtaque)
      /*
       * goku pega 20, krilin pega 200 y se pega 100
       *
       * start: goku: 9000 - krilin: 5000
       *
       * round 1: goku   -> muchosgolpes  -  goku 9000, krilin 4980
       *          krilin -> kamehameha    -  goku 8800, krilin 4880
       *
       * round 1: goku   -> muchosgolpes  -  goku 8800, krilin 4860
       *          krilin -> kamehameha    -  goku 8600, krilin 4760
       *
       * round 1: goku   -> muchosgolpes  -  goku 8600, krilin 4740
       *          krilin -> kamehameha    -  goku 8400, krilin 4640
       *
       * end: goku: 8400 - krilin: 4640
       */

      val(g1, k1) = res.get.asInstanceOf[SiguenPeleando].guerreros
      assert(g1 === goku.copiarConEnergia(8400) && k1 === krilin.copiarConEnergia(4640))
    }
  }

  test("Plan de ataque") {
    new GuerreroTest {
      val plan = goku.planDeAtaqueContra(vegeta, 7)(MayorDanio)
      assert(plan === Some(Seq.fill(7)(MuchosGolpes)))
    }
  }

  test("Plan de ataque 2") {
    new GuerreroTest {
      val plan = vegeta.planDeAtaqueContra(goku, 7)(MenorDanio)
      assert(plan === Some(Seq.fill(7)(Cargar)))
    }
  }



}