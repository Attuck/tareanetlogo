;; Each potential solution is represented by a turtle.

turtles-own [
  bits           ;; list of 0's and 1's
  fitness
]

globals [
  winner         ;; turtle that currently has the best solution
  fitness_p1
  fitness_p2
]

to setup
  clear-all
  create-turtles population-size [
    set rnd [one-of [true false]
      ifelse rnd
      [set bits n-values world-width [one-of [0 0 0 0 0 0 0 0 0 1]]]
      [set bits n-values world-width [one-of [0 1 1 1 1 1 1 1 1 1]]]
    ifelse fitness-function?
    [calculate-fitness]
    [calculate-fitness-2]
    hide-turtle  ;; the turtles' locations are not used, so hide them
  ]
  ;;file-delete "log_netlogo.txt"
  update-display
  reset-ticks
end
to log_
  file-open (word "log_netlogo" CROSSOVER-RATE MUTATION-RATE ".txt")
  file-write [fitness] of winner
  file-write diversity
  file-type "\n"
  file-close
end

to go
  log_
  if [fitness] of winner = world-width
    [ stop ]
  create-next-generation
  update-display
  tick
end

to update-display
  set winner max-one-of turtles [fitness]
  ask patches [
    ifelse item pxcor ([bits] of winner) = 1
      [ set pcolor white ]
      [ set pcolor black ]
  ]
end

;; ===== Generating Solutions

;; Each solution has its "fitness score" calculated.
;; Higher scores mean "more fit", and lower scores mean "less fit".
;; The higher a fitness score, the more likely that this solution
;;   will be chosen to reproduce and create offspring solutions
;;   in the next generation.
;;
to calculate-fitness       ;; turtle procedure
  ;; For the "ALL-ONES" problem, the fitness is simply equal to the number of ones
  ;; that occur in this solution's bits.
  ;; However, you could solve more interesting problems by changing this procedure
  ;; to evaluate the bits in other ways.  For instance, the bits might
  ;; encode rules for how a turtle should move across the world in a search for food.
  set fitness length (remove 0 bits)
end

to calculate-fitness-2
  let unos length (remove 0 bits)
  let ceros length (remove 1 bits)
  let diferencia (unos - ceros)
  let total (100 - (absolute-value diferencia))
  set fitness total
  ;;print total
end

to-report absolute-value [number]
  ifelse number >= 0
  [report number]
  [report (- number)]
end
;; This procedure does the main work of the genetic algorithm.
;; We start with the old generation of solutions.
;; We choose solutions with good fitness to produce offspring
;; through crossover (sexual recombination), and to be cloned
;; (asexual reproduction) into the next generation.
;; There is also a chance of mutation occurring in each individual.
;; After a full new generation of solutions has been created,
;; the old generation dies.
to create-next-generation
  ; The following line of code looks a bit odd, so we'll explain it.
  ; if we simply wrote "LET OLD-GENERATION TURTLES",
  ; then OLD-GENERATION would mean the set of all turtles, and when
  ; new solutions were created, they would be added to the breed, and
  ; OLD-GENERATION would also grow.  Since we don't want it to grow,
  ; we instead write "TURTLES WITH [TRUE]", which makes OLD-GENERATION
  ; an agentset, which doesn't get updated when new solutions are created.
  let old-generation turtles with [true]

  ; Some number of the population is created by crossover each generation
  ; we divide by 2 because each time through the loop we create two children.
  let crossover-count  (floor (population-size * crossover-rate / 100 / 2))

  repeat crossover-count
  [
    ; We use "tournament selection", with tournament size = 3.
    ; This means, we randomly pick 3 solutions from the previous generation
    ; and select the best one of those 3 to reproduce.

    let parent1 max-one-of (n-of 3 old-generation) [fitness]
    let parent2 max-one-of (n-of 3 old-generation) [fitness]
    set fitness_p1 [fitness] of parent1
    set fitness_p2 [fitness] of parent2
    let child-bits crossover ([bits] of parent1) ([bits] of parent2)

    ; create the two children, with their new genetic material
    ask parent1 [ hatch 1 [ set bits item 0 child-bits ] ]
    ask parent2 [ hatch 1 [ set bits item 1 child-bits ] ]
  ]

  ; the remainder of the population is created by cloning
  ; selected members of the previous generation
  repeat (population-size - crossover-count * 2)
  [
    ask max-one-of (n-of 3 old-generation) [fitness]
      [ hatch 1 ]
  ]

  ask old-generation [ die ]

  ; now we're just talking to the new generation of solutions here
  ask turtles
  [
    ; there's a chance of mutations occurring
    mutate
    ; finally we update the fitness value for this solution
    ifelse fitness-function? [calculate-fitness] [calculate-fitness-2]
  ]
end

;; ===== Mutations

;; This reporter performs one-point crossover on two lists of bits.
;; That is, it chooses a random location for a splitting point.
;; Then it reports two new lists, using that splitting point,
;; by combining the first part of bits1 with the second part of bits2
;; and the first part of bits2 with the second part of bits1;
;; it puts together the first part of one list with the second part of
;; the other.
to-report crossover [bits1 bits2]
  let split-point 1 + random (length bits1 - 1)
  report list (sentence (sublist bits1 0 split-point)
                        (sublist bits2 split-point length bits2))
              (sentence (sublist bits2 0 split-point)
                        (sublist bits1 split-point length bits1))
end

;; This procedure causes random mutations to occur in a solution's bits.
;; The probability that each bit will be flipped is controlled by the
;; MUTATION-RATE slider.
to mutate   ;; turtle procedure
  set bits map [ [b] ->
    ifelse-value (random-float 100.0 < mutation-rate)
      [ 1 - b ]
      [ b ]
  ] bits
end

;; ===== Diversity Measures

;; Our diversity measure is the mean of all-pairs Hamming distances between
;; the genomes in the population.
to-report diversity
  let distances []
  ask turtles [
    let bits1 bits
    ask turtles with [self > myself] [
      set distances fput (hamming-distance bits bits1) distances
    ]
  ]
  ; The following  formula calculates how much 'disagreement' between genomes
  ; there could possibly be, for the current population size.
  ; This formula may not be immediately obvious, so here's a sketch of where
  ; it comes from.  Imagine a population of N turtles, where N is even, and each
  ; turtle has  only a single bit (0 or 1).  The most diverse this population
  ; can be is if half the turtles have 0 and half have 1 (you can prove this
  ; using calculus!). In this case, there are (N / 2) * (N / 2) pairs of bits
  ; that differ.  Showing that essentially the same formula (rounded down by
  ; the floor function) works when N is odd, is left as an exercise to the reader.
  let max-possible-distance-sum floor (count turtles * count turtles / 4)

  ; Now, using that number, we can normalize our diversity measure to be
  ; between 0 (completely homogeneous population) and 1 (maximally heterogeneous)
  report (sum distances) / max-possible-distance-sum
end

;; The Hamming distance between two bit sequences is the fraction
;; of positions at which the two sequences have different values.
;; We use MAP to run down the lists comparing for equality, then
;; we use LENGTH and REMOVE to count the number of inequalities.
to-report hamming-distance [bits1 bits2]
  report (length remove true (map [ [b1 b2] -> b1 = b2 ] bits1 bits2)) / world-width
end


; Copyright 2008 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
24
17
762
55
-1
-1
7.3
1
10
1
1
1
0
1
1
1
0
99
0
3
1
1
1
ticks
30.0

BUTTON
108
108
193
141
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
20
68
193
101
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
20
148
192
181
population-size
population-size
5
200
110.0
5
1
NIL
HORIZONTAL

PLOT
200
68
530
218
Fitness Plot
gen #
raw fitness
0.0
20.0
0.0
101.0
true
true
"" ""
PENS
"best" 1.0 0 -2674135 true "" "plot max [fitness] of turtles"
"avg" 1.0 0 -10899396 true "" "plot mean [fitness] of turtles"
"worst" 1.0 0 -13345367 true "" "plot min [fitness] of turtles"

BUTTON
20
108
105
141
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
20
228
192
261
mutation-rate
mutation-rate
0
10
0.09
0.01
1
NIL
HORIZONTAL

PLOT
200
223
532
380
Diversity Plot
gen #
diversity
0.0
20.0
0.0
1.0
true
false
"" ""
PENS
"diversity" 1.0 0 -8630108 true "" "if plot-diversity? [ plot diversity ]"

SWITCH
20
268
192
301
plot-diversity?
plot-diversity?
0
1
-1000

SLIDER
20
188
192
221
crossover-rate
crossover-rate
0
100
30.0
1
1
NIL
HORIZONTAL

SWITCH
19
311
190
344
plot-parents-fitness?
plot-parents-fitness?
0
1
-1000

PLOT
201
399
401
549
Parents Fitness
gen #
fitness
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -8630108 true "" "if plot-parents-fitness? [plot fitness_p1]"
"pen-1" 1.0 0 -5825686 true "" "if plot-parents-fitness? [plot fitness_p2]"

SWITCH
30
371
178
404
fitness-function?
fitness-function?
1
1
-1000

@#$#@#$#@
## ¿Qué es el modelo?

Este modelo demuestra el uso de un algoritmo genético en un problema muy simple. Los algoritmos genéticos (AG) son una técnica de ciencias de la computación inspirada en la biología que combina nociones de la genética mendeliana y la evolución darwiniana para buscar buenas soluciones a los problemas. La AG funciona generando una población aleatoria de soluciones a un problema, evaluando esas soluciones con una función de aptitud y luego utilizando clonación, recombinación y mutación para crear nuevas soluciones al problema.

En este modelo, utilizamos el sencillo problema "ALL-ONES" para demostrar cómo esto es posible. Usamos un problema tan simple en este modelo para resaltar solo la técnica de solución. La idea del problema "ALL-ONES" es encontrar una cadena de bits (es decir, una secuencia de unos y ceros) que contiene todos los unos, y no hay ceros. Por lo tanto, la cadena que mejor resuelve este problema es "111111 ... 111".

## ¿Cómo funciona?

Este algoritmo genético se compone de los siguientes pasos.

1) Se crea una población de soluciones aleatorias. Cada solución consiste en una cadena de "1" s y "0" s elegidos aleatoriamente.

2) Cada solución se evalúa sobre la base de qué tan bien resuelve el problema. Esta medida se llama su "aptitud". En este modelo, nuestro objetivo es simplemente encontrar una solución que consista en todos los "1" s. Existe una medida de aptitud alternativa que es máxima cuando la solución es más diversa, o sea, cuando hay igual cantidad de unos y de ceros.

3) Se crea una nueva generación de soluciones a partir de la generación anterior, donde es más probable que las soluciones que tienen puntajes de aptitud más altos se elijan como soluciones "padre" que aquellas que tienen puntajes de aptitud bajos.

A) El método de selección utilizado en este modelo se llama "selección de torneo", con un tamaño de torneo de 3. Esto significa que 3 soluciones se obtienen al azar de la generación anterior, y la que tiene la aptitud más alta se elige para convertirse en padre.

B) Se elige uno o dos padres para crear hijos. Con uno de los padres, el hijo es un clon o copia del padre. Con dos padres, el proceso es el análogo digital de la recombinación sexual: los dos hijos heredan parte de su material genético de un padre y parte del otro.

C) También existe la posibilidad de que ocurra la mutación, y algunos de los bits del hijo se cambiarán de "1" a "0" s, o viceversa.

4) Los pasos 2 y 3 anteriores se repiten hasta que se encuentre una solución que resuelva el problema con éxito.

## ¿Cómo se usa?

Presione el botón SETUP para crear una población aleatoria inicial de soluciones.

Presione el botón STEP para tener una nueva generación creada a partir de la generación anterior.

Presione el botón GO para ejecutar el algoritmo genético hasta que se encuentre una solución.

La mejor solución encontrada en cada generación se muestra en la VISTA. Cada columna blanca representa un bit en "1" y cada columna negra representa un bit en"0".

=== Parámetros ===

El control deslizante de  POPULATION-SIZE  controla el número de soluciones que están presentes en cada generación.

El control deslizante CROSSOVER-RATE controla qué porcentaje de cada nueva generación se crea a través de la reproducción sexual (recombinación o cruzamiento entre el material genético de dos padres) y qué porcentaje (100 - CROSSOVER-RATE) se crea a través de la reproducción asexual (clonación material).

El control deslizante MUTATION-RATE controla el porcentaje de posibilidad de mutación. Esta oportunidad se aplica a cada posición en la cadena de bits de un nuevo individuo. Por ejemplo, si la cadena tiene 100 bits de longitud y la tasa de mutación se establece en 1%, en promedio se cambiará un bit durante la creación de cada nuevo individuo.

El switch PLOT-DIVERSIDAD?  controla si la cantidad de diversidad dentro de la población de soluciones se traza en cada generación, como se muestra en el "Diagrama de diversidad". Desactivar PLOT-DIVERSITY? aumenta significativamente la velocidad del modelo porque el cálculo de la diversidad requiere una gran cantidad de computación.

El "Gráfico de condición física" se utiliza para mostrar los mejores, medios y peores valores de condición física de las soluciones en cada generación.

## ¿Cómo extender el modelo?

Existen muchas variaciones en este algoritmo genético simple. Por ejemplo, algunos algoritmos genéticos incluyen "elitismo". En este caso, el mejor X% de soluciones de la generación anterior siempre se copia directamente en la nueva generación. Modifique este modelo para que use elitismo.

Otro tipo de selección para la reproducción que a veces se usa en algoritmos genéticos se llama "selección de ruleta". En este caso, puede imaginarse que a cada solución de la población se le asigna una cuña de una rueda de ruleta grande. El tamaño de la cuña se determina dividiendo la aptitud de cada solución por la suma de las capacidades de todas las soluciones de la población. Por lo tanto, la probabilidad de seleccionar cualquier solución dada para reproducir es directamente proporcional a su aptitud. Intente implementar este tipo de selección y compare su rendimiento con el método de "selección de torneo" que se usa actualmente en este modelo.

Como se señaló anteriormente, el problema "TODAS LAS UNIDAS" es un problema de juguete que no es muy interesante en sí mismo. Una extensión natural de este modelo es utilizar el algoritmo genético para resolver un problema que es significativamente más interesante. Afortunadamente, puedes cambiar el problema que el algoritmo genético está resolviendo modificando solo una cosa, la "función de aptitud", que evalúa qué tan buena es una cadena de bits dada para resolver cualquier problema que intentes resolver. Por ejemplo, podrías desarrollar reglas sobre cómo debería moverse una tortuga para maximizar su colección de alimentos a medida que viaja por el mundo. Para hacerlo, puedes cambiar el procedimiento `ga-calculate-fitness` para ejecutar una pequeña simulación donde una tortuga se mueve en el mundo (de acuerdo con algunas reglas que están definidas por la cadena de" 1 "sy" 0 "s) , cuente la cantidad de comida que recolecta la tortuga, y luego configure la aptitud de acuerdo a esto.


## Créditos y referencias

Este modelo se basa en el trabajo de  Stonedahl, F. y Wilensky, U., que se basó en John H. Holland, que es ampliamente considerado como el padre de los algoritmos genéticos. Véase el libro de Holland "Adaptación en sistemas naturales y artificiales", 1992, MIT Press.

Ver cita:
Stonedahl, F. and Wilensky, U. (2008).  NetLogo Simple Genetic Algorithm model.  http://ccl.northwestern.edu/netlogo/models/SimpleGeneticAlgorithm.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL

## ¿Cómo citar?

Si cita este modelo o el software NetLogo en una publicación, le pedimos que incluya las citas a continuación.

Para el modelo en sí:

* Apu, R. y Oscar, C. (2017). Modelo de Algoritmo Genético Simple de NetLogo adaptado. hhttps: //github.com/Attuck/tareanetlogo. Escuela de Ciencias de Computación e Informática, Universidad de Costa Rica, San José, Costa Rica.


Por favor, cite el software NetLogo como:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Centro de aprendizaje conectado y modelado basado en computadora, Northwestern University, Evanston, IL.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.1
@#$#@#$#@
need-to-manually-make-preview-for-this-model
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
