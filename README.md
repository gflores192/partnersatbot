Este repositorio tiene como fin hallar una manera de encontrar los thresholds de ongoing orders de tiendas para el "Partner Saturation Bot".

En la actualidad se usa el siguiente reporte: https://docs.google.com/spreadsheets/d/1xWTd9expnFoGeBeKf5tmgWbUh9vobo4hOvNOxzgNVvQ/edit#gid=2092332451

Para poder hallar los thresholds se tiene que realizar el proceso de manera manual, por lo que se tienen los siguientes problemas:
1. Hallar el threshold adecuado para cada tienda (que minimize ordenes perdidas y minimize el waiting time pickup).
2. Enfocar el bot a las tiendas que más afectan a la operación.

Para esto se ha realizado un script en R que se alimenta del siguiente query: https://glovoapp.eu.looker.com/sql/skpqzkwgd9rqcs
Nota: El archivo csv del repositorio es el resultado del query para todo Junio 2020 en PE.

El script se encarga de transformar el output del query a un formato más sencillo en el cual se puede visualizar lo siguiente:
1. Tiendas.
2. Ordenes entregadas y canceladas por tienda (con acumulado y porcentaje).
3. Tiempo de dispatching, aceptación, waiting time y preparation time.
4. R2 de cada indicador (delivered, orders, tiempos) en relación a las ongoing orders.
Todos estos kpis se ven en base al rango de ongoing orders.

El output del script se puede ver en el siguiente reporte (hoja: InfoGroup): https://docs.google.com/spreadsheets/d/1qSCZlqdvhc2M4vxqrzmZ4pO-pcO2eij0L7f3fdTV4BM/edit?ts=5eff5eb9#gid=1755727382

En resumen, por el momento tenemos lo siguiente:
- Variación de los kpis por rango de ongoing orders para las tiendas top por país o ciudad.
- Se pueden calcular los thresholds en base a los kpis de una manera aún manual pero que requiere menos tiempo.

Por realizar:
- Encontrar función de minimización de pérdida de orders en relación a los tiempos (ongoing orders). Por ahora solo wtp y si es posible todos los tiempos.
- Tener umbral de manera automática en el archivo como una columna más por cada kpi para realizar el proceso de manera masiva.
