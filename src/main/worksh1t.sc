1 + 1

import Guerrero._
import Movimiento._
import Criterio._
import Item._
import ResultadoPelea._

val listaItems = List(SemillaDelErmitanio, SemillaDelErmitanio)
val listaMovimientos = List[Movimiento](Cargar, UsarItem(SemillaDelErmitanio), MuchosGolpes)
val caracteristicasGoku = Caracteristicas("Goku", listaItems, listaMovimientos, 9999, 9000)
val goku = Saiyajin(caracteristicasGoku, cola = false, 0, estadoMono = false)

val listaItemsVegeta = List(SemillaDelErmitanio, SemillaDelErmitanio)
val listaMovimientosVegeta = List[Movimiento](Cargar, UsarItem(SemillaDelErmitanio), MuchosGolpes)
val caracteristicasVegeta = Caracteristicas("Vegeta", listaItemsVegeta, listaMovimientosVegeta, 9500, 8900)
val vegeta = Saiyajin(caracteristicasVegeta, cola = false, 0, estadoMono = false)

val loquesea = goku.planDeAtaqueContra(vegeta, 3)(MenorDanio)

goku.pelearContra(vegeta)(loquesea)
print("ASDASDAS")