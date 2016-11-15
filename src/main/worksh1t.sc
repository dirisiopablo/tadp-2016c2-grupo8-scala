import Guerrero._
import Movimiento._
import Criterio._
import Item._

val listaItemsGoku = List()
val listaMovimientosGoku = List(MuchosGolpes)
val caracteristicasGoku = Caracteristicas("Goku", listaItemsGoku, listaMovimientosGoku, 9999, 9000)
val goku = Saiyajin(caracteristicasGoku, cola = false, 0, estadoMono = false)

val listaItemsVegeta = List()
val listaMovimientosVegeta = List[Movimiento](Cargar, UsarItem(SemillaDelErmitanio), MuchosGolpes)
val caracteristicasVegeta = Caracteristicas("Vegeta", listaItemsVegeta, listaMovimientosVegeta, 150, 10)
val vegeta = Saiyajin(caracteristicasVegeta, cola = false, 0, estadoMono = false)

val plan = goku.planDeAtaqueContra(vegeta, 4)(MayorDanio)

goku.pelearContra(vegeta)(plan)
