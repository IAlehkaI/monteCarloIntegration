package MonteCarloIntegration

import java.util.concurrent.Executors
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.Random

object MonteCarloIntegration {

  def integralMonteCarlo(
                          f: Double => Double,
                          l: Double,
                          r: Double,
                          pointsNumber: Int,
                          threadsNumber: Int
                        ): Double = {

    // 1. Создаем ExecutionContext с фиксированным количеством потоков
    val executor = Executors.newFixedThreadPool(threadsNumber)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

    try {
      // 2. Находим ограничивающий прямоугольник для функции
      val searchPoints = 1000
      val step = (r - l) / searchPoints

      val (minVal, maxVal) = (0 to searchPoints).foldLeft((Double.MaxValue, Double.MinValue)) {
        case ((minAcc, maxAcc), i) =>
          val x = l + i * step
          val y = f(x)
          (math.min(minAcc, y), math.max(maxAcc, y))
      }

      val height = maxVal - minVal
      val baseY = minVal

      // 3. Распределяем точки по Future
      val pointsPerTask = pointsNumber / threadsNumber
      val remainingPoints = pointsNumber % threadsNumber

      // Создаем список Future, каждая считает свои точки
      val futures = (0 until threadsNumber).map { taskId =>
        Future {
          val random = new Random()
          val pointsToProcess = pointsPerTask + (if (taskId < remainingPoints) 1 else 0)

          var localSuccess = 0L

          for (_ <- 0 until pointsToProcess) {
            val x = l + (r - l) * random.nextDouble()
            val y = baseY + height * random.nextDouble()
            val fx = f(x)

            // Проверяем, находится ли точка под графиком функции
            if ((fx >= baseY && y <= fx) || (fx < baseY && y >= fx)) {
              localSuccess += 1
            }
          }

          localSuccess
        }
      }

      // 4. Агрегируем результаты всех Future
      val totalSuccessFuture = Future.foldLeft(futures)(0L)(_ + _)

      // 5. Ждем завершения и вычисляем результат
      val totalSuccess = Await.result(totalSuccessFuture, Duration.Inf)
      val rectangleArea = (r - l) * height
      val ratio = totalSuccess.toDouble / pointsNumber

      val signedArea = rectangleArea * ratio

      signedArea

    } finally {
      // 6. Завершаем Executor
      executor.shutdown()
    }
  }

  // Пример использования
  def main(args: Array[String]): Unit = {
    def parabola(x: Double): Double = x * x

    def sinFunc(x: Double): Double = math.sin(x)

    def expFunc(x: Double): Double = math.exp(x)

    println("Интегрирование методом Монте-Карло с использованием Future")

    // Параметры для всех тестов
    val totalPoints = 2000000
    val threads = 4

    println(s"Количество точек: $totalPoints")
    println(s"Количество потоков: $threads")
    println()

    // Тест 1
    println("1. x² dx от 0 до 2:")
    val start1 = System.currentTimeMillis()
    val result1 = integralMonteCarlo(parabola, 0, 2, totalPoints, threads)
    val time1 = System.currentTimeMillis() - start1
    val exact1 = 8.0 / 3
    println(f"  Результат: $result1%.6f")
    println(f"  Точное значение: $exact1%.6f")
    println(f"  Погрешность: ${math.abs(result1 - exact1)}%.6f")
    println(f"  Время: ${time1}ms")
    println()

    // Тест 2
    println("2. sin(x) dx от 0 до π:")
    val start2 = System.currentTimeMillis()
    val result2 = integralMonteCarlo(sinFunc, 0, math.Pi, totalPoints, threads)
    val time2 = System.currentTimeMillis() - start2
    val exact2 = 2.0
    println(f"  Результат: $result2%.6f")
    println(f"  Точное значение: $exact2%.6f")
    println(f"  Погрешность: ${math.abs(result2 - exact2)}%.6f")
    println(f"  Время: ${time2}ms")
    println()

    // Тест 3
    println("3. e^x dx от 0 до 1:")
    val start3 = System.currentTimeMillis()
    val result3 = integralMonteCarlo(expFunc, 0, 1, totalPoints, threads)
    val time3 = System.currentTimeMillis() - start3
    val exact3 = math.E - 1
    println(f"  Результат: $result3%.6f")
    println(f"  Точное значение: $exact3%.6f")
    println(f"  Погрешность: ${math.abs(result3 - exact3)}%.6f")
    println(f"  Время: ${time3}ms")
    println()

  }
}