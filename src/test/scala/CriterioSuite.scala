import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Guerrero._
import Movimiento._
import Criterio._
import Item._

@RunWith(classOf[JUnitRunner])
class CriterioSuite extends FunSuite {

  trait CriterioTest {
    val listaItemsGoku = List()
    val listaMovimientosGoku = List(MuchosGolpes)
    val caracteristicasGoku = Caracteristicas("Goku", listaItemsGoku, listaMovimientosGoku, 9999, 9000)
    val goku = Guerrero(caracteristicasGoku, Saiyajin(cola = false, nivelSaiyajin = 0, estadoMono = false, inconsciente = true))

    val listaItemsVegeta = List(SemillaDelErmitanio)
    val listaMovimientosVegeta = List[Movimiento](UsarItem(SemillaDelErmitanio), MuchosGolpes)
    val caracteristicasVegeta = Caracteristicas("Vegeta", listaItemsVegeta, listaMovimientosVegeta, 150, 150)
    val vegeta = Guerrero(caracteristicasVegeta, Saiyajin(cola = false, nivelSaiyajin = 0, estadoMono = false, inconsciente = true))

    val listaItems18 = List()
    val listaMovimientos18 = List(Explotar)
    val caracteristicas18 = Caracteristicas("18", listaItems18, listaMovimientos18, 8, 2)
    val a18 = Guerrero(caracteristicas18, Androide())
  }

  test("Mayor daño") {
    new CriterioTest {
      val mov = goku.movimentoMasEfectivoContra(vegeta)(MayorDanio)
      assert(mov === Some(MuchosGolpes))
    }
  }

  test("Menor daño") {
    new CriterioTest {
      val mov = vegeta.movimentoMasEfectivoContra(goku)(MenorDanio)
      assert(mov === Some(UsarItem(SemillaDelErmitanio)))
    }
  }

  test("Menor desventaja") {
    new CriterioTest {
      val mov = goku.movimentoMasEfectivoContra(vegeta)(MenorDesventaja)
      assert(mov === Some(MuchosGolpes))
    }
  }

  test("Gastar menos items") {
    new CriterioTest {
      val mov = vegeta.movimentoMasEfectivoContra(goku)(GastarMenosItems)
      assert(mov === Some(MuchosGolpes))
    }
  }

  test("Que no me mate") {
    new CriterioTest {
      val mov = a18.movimentoMasEfectivoContra(goku)(QueNoMeMate)
      assert(mov === None)
    }
  }

}