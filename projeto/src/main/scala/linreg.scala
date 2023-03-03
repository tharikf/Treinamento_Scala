
// Constructing a class that implement simple linear regression

// y = b_{0} + b_{1} * X + u_{i} = x^{T} * B + u_{i}

import breeze.linalg.{DenseMatrix, DenseVector}
import scala.util.Random

class LinearRegression(var X: Array[Array[Double]], var Y: Array[Double]) {

    var matrix_X = new DenseMatrix(X.length, X.head.length, X.flatten)
    var vector_Y = new DenseVector(Y)
    val x_0 = X(0) // Array(2, 3)

    
    def parameters(): DenseVector[Double] = {

        val alpha: Double = 0.01 // val can't be replace
        val iterations: Int = 200

        var ones = DenseVector.ones[Double](matrix_X.rows)
        val comp_matrix_X = DenseMatrix.horzcat(matrix_X, ones.toDenseMatrix.t)
        val coefs = DenseVector.fill(comp_matrix_X.cols) {Random.nextDouble()}

        for (i <- 0 until iterations) {

            val hypothesis = comp_matrix_X * coefs
            val loss = hypothesis - vector_Y
            val gradient = (comp_matrix_X.t * loss) / matrix_X.rows.toDouble
            coefs -= alpha * gradient
        }

        coefs
    }
}

object PrintarResultado {

    def main(args: Array[String]): Unit = {
        val x: Array[Array[Double]] = Array(Array(2, 3), Array(5, 4), Array(4, 8), Array(8, 14), Array(7, 10), Array(15, 20),
            Array(30, 12), Array(20, 10), Array(15, 27), Array(20, 35))
        val y: Array[Double] = Array(3, 4, 5, 11, 18, 17, 39, 21, 21, 15)

        val reg_lin = new LinearRegression(x, y)
        val parametro: DenseVector[Double] = reg_lin.parameters()

        println(s"Os parametros são: $parametro")
    }
}


