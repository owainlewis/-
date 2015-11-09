# Shapes

Exotic scala types and shapes

```scala

import io.forward.polyfunctors._

object Demo {
  val inc = (x: Int) => x + 1

  val result = Bifunctor[Tuple2].bimap((1,2), inc, inc)
}

```
