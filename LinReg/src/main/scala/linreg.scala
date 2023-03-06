
// Constructing a class of multidimensional linear regression

// y = b_{0} + b_{1} * X + u_{i} = x^{T} * B + u_{i}

import breeze.linalg.{DenseMatrix, DenseVector}
import scala.util.Random

class LinearRegression(var X: Array[Array[Double]], var Y: Array[Double]) {

    val X_transpose = X.transpose // Performing transpose of multidimensional vector X
    val matrix_X = new DenseMatrix(X_transpose.length, X_transpose.head.length, X_transpose.flatten) // Transforming multidimensional vector into dense matrix
    val vector_Y = new DenseVector(Y)

    
    def parameters(alpha: Double = 0.01, iterations: Int = 200): DenseVector[Double] = {

        val ones = DenseVector.ones[Double](matrix_X.rows)
        val comp_matrix_X = DenseMatrix.horzcat(matrix_X, ones.toDenseMatrix.t)
        var coefs = DenseVector.zeros[Double](comp_matrix_X.cols) // Initialize coefficents with zeros

        for (i <- 0 until iterations) {

            var hypothesis = comp_matrix_X * coefs
            var loss = hypothesis - vector_Y
            var gradient = comp_matrix_X.t * loss * (1.0 / vector_Y.length)
            coefs -= alpha * gradient
        }
        coefs
    }
}


object Implementing {

    def main(args: Array[String]): Unit = {
        val x: Array[Array[Double]] = Array(Array(2, 5, 4, 8, 7, 15, 30, 20, 15, 20), Array(3, 4, 8, 14, 10, 20, 12, 10, 27, 35))
        val y: Array[Double] = Array(3, 4, 5, 11, 18, 17, 39, 21, 21, 15)

        val lin_reg = new LinearRegression(x, y)
        val parameters: DenseVector[Double] = lin_reg.parameters(alpha = 0.001, iterations = 2000)


        println(s"The parameters are $parameters")
    }
}


