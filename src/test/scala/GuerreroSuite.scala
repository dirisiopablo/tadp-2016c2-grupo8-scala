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
    val goku = Saiyajin(caracteristicasGoku, cola = false, 0, estadoMono = false)

    val listaItemsVegeta = List(SemillaDelErmitanio)
    val listaMovimientosVegeta = List[Movimiento](Cargar, UsarItem(SemillaDelErmitanio), MuchosGolpes)
    val caracteristicasVegeta = Caracteristicas("Vegeta", listaItemsVegeta, listaMovimientosVegeta, 8000, 1000)
    val vegeta = Saiyajin(caracteristicasVegeta, cola = false, 0, estadoMono = false)
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
      val res = goku.pelearContra(vegeta)(goku.planDeAtaqueContra(vegeta, 6)(MayorDanio))
      /*
       * round 1: goku -> muchosgolpes, vegeta -> semilla   -   goku 9000, vegeta 8000
       * round 2: goku -> muchosgolpes, vegeta -> cargar   -    goku 9000, vegeta 8000
       * round 3: goku -> muchosgolpes, vegeta -> cargar   -    goku 9000, vegeta 8000
       * round 4: goku -> muchosgolpes, vegeta -> cargar   -    goku 9000, vegeta 8000
       * round 5: goku -> muchosgolpes, vegeta -> cargar   -    goku 9000, vegeta 8000
       * round 6: goku -> muchosgolpes, vegeta -> cargar   -    goku 9000, vegeta 8000
       */
      val lastGoku = goku
      val lastVegeta = vegeta copiarConItems List() copiarConEnergia 8000

      assert(res === Some(SiguenPeleando(goku, lastVegeta)))
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