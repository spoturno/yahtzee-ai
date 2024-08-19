# Yahtzee AI - Prolog Implementation

Este proyecto consiste en la implementación de una inteligencia artificial para jugar al Yahtzee, un juego de dados clásico. El proyecto fue desarrollado como parte de un laboratorio de Programación Lógica en Prolog.

## Descripción

Yahtzee es un juego de dados en el cual los jugadores lanzan cinco dados para intentar conseguir ciertas combinaciones con el objetivo de maximizar su puntaje. Esta implementación en Prolog permite jugar contra una inteligencia artificial que sigue diferentes estrategias de juego, incluyendo una estrategia probabilística avanzada.

## Reglas del Juego

El juego se desarrolla en 13 rondas. En cada ronda, se lanzan los cinco dados y el jugador puede elegir relanzar algunos o todos los dados hasta dos veces adicionales. Después de los lanzamientos, el jugador debe seleccionar una de las 13 categorías disponibles y registrar su puntaje para esa ronda. El objetivo es maximizar el puntaje final acumulado en las categorías.

### Categorías

- **Sección Superior**:
  - Aces: Suma de dados que muestren 1.
  - Twos: Suma de dados que muestren 2.
  - Threes: Suma de dados que muestren 3.
  - Fours: Suma de dados que muestren 4.
  - Fives: Suma de dados que muestren 5.
  - Sixes: Suma de dados que muestren 6.
  - **Bonus**: Si se alcanza un total de 63 puntos en la sección superior, se otorgan 35 puntos adicionales.

- **Sección Inferior**:
  - Three of a kind: Al menos tres dados iguales. Suma de todos los dados.
  - Four of a kind: Al menos cuatro dados iguales. Suma de todos los dados.
  - Full House: Tres dados iguales y dos dados iguales. 25 puntos.
  - Small Straight: Cuatro dados en secuencia. 30 puntos.
  - Large Straight: Cinco dados en secuencia. 40 puntos.
  - Yahtzee: Cinco dados iguales. 50 puntos.
  - Chance: Cualquier combinación. Suma de todos los dados.

## Implementación

La solución se implementa en Prolog utilizando varias estrategias para seleccionar la mejor jugada en cada turno. Las estrategias disponibles son:

- **Humano**: El jugador elige manualmente qué dados relanzar y en qué categoría registrar el puntaje.
- **IA Determinista (ia_det)**: La IA sigue reglas predefinidas para maximizar el puntaje, priorizando las combinaciones más valiosas.
- **IA Probabilística (ia_prob)**: La IA evalúa las probabilidades de las diferentes combinaciones posibles para tomar decisiones más informadas.

### Predicados Principales

- `puntaje(+Dados, +Cat, -Puntos)`: Calcula el puntaje para una categoría específica basada en los dados actuales.
- `puntaje_tablero(+Tablero, -Puntaje)`: Calcula el puntaje total de un tablero completo.
- `ajustar_tablero(+Tablero, +Categoria, +Puntaje, -TableroSalida)`: Asigna el puntaje de una categoría específica al tablero.
- `cambio_dados(+Dados, +Tablero, +Estrategia, -Patron)`: Determina qué dados relanzar basándose en la estrategia elegida.
- `eleccion_slot(+Dados, +Tablero, +Estrategia, -Categoria)`: Selecciona la categoría para asignar el puntaje basado en la estrategia de juego.

## Ejecución

### Prerrequisitos

- SWI-Prolog instalado en el sistema.
- Problog configurado correctamente en el entorno.

### Cómo jugar

1. **Jugador Humano**: 
   ```prolog
   yahtzee(humano, Seed).
   ```

2. **IA Determinista**:
   ```prolog
   yahtzeelog(ia_det, Seed).
   ```
3. **IA Probabilista**:
   ```prolog
   yahtzeelog(ia_prob, Seed).
   ```
En todos los casos, `Seed` es un número utilizado para inicializar el generador de números aleatorios.

## Resultados y Estadísticas
El archivo informe.pdf adjunto contiene un análisis detallado de las estrategias implementadas, junto con la media y la desviación estándar de los puntajes obtenidos en 20 partidas con cada estrategia.
