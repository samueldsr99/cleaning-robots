# agents

## Agente Reactivo

### Reglas

1.2 - Esta cargando un niño && La celda actual es un corral: RDropChild
1.3 - Esta cargando un niño && La celda actual esta sucia: RClean
1.4 - Esta cargando un niño: Llevar al niño al corral vacio mas cercano

1.5 - No esta cargando un niño && no hay niños fuera del corral && la celda actual esta sucia: RClean
1.6 - No esta cargando un niño && no hay niños fuera del corral && existen celdas sucias: Buscar la celda sucia mas cercana
1.7 - No esta cargando un niño: Buscar al niño fuera del corral que mas rapido pueda llevar a un corral vacio

1.8 - True: RStay
