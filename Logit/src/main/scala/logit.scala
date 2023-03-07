
// Constructing a class of logit algorithm

import scala.math._
import breeze.linalg.{DenseMatrix, DenseVector}

class Logit(var X: Array[Array[Double]], var Y: Array[Int]) {

    val X_transpose = X.transpose
    val matrix_X = new DenseMatrix(X_transpose.length, X_transpose.head.length, X_transpose.flatten)
    val vector_Y = new DenseVector(Y)


    def parameters(learning_rate: Double = 0.01, iterations: Int = 500): DenseVector[Double] = {

        val n_samples: Int = vector_Y.length
        require(n_samples > 0, "Sample has to be greater than zero!") // 

        var coefs = DenseVector.zeros[Double](X_transpose.head.length)
        var bias: Double = 0

        for (i <- 0 until iterations) {

            var linear_model = matrix_X * coefs + bias
            var y_predict = linear_model.map(x => 1.0 / (1.0 + exp(-x)))
            var loss = (y_predict - vector_Y.map(_.toDouble))

            var gradient_coefs = (1.0 / n_samples) * (matrix_X.t * loss)
            var gradient_bias = (1.0 / n_samples) * (loss).sum

            coefs -= (learning_rate * gradient_coefs)
            bias -= (learning_rate * gradient_bias)
        }
        val estimated_parameters = DenseVector.vertcat(coefs, DenseVector(bias))
        estimated_parameters
    }
}


object Implementing {

    def main(args: Array[String]): Unit = {
        val x: Array[Array[Double]] = Array(Array(5, 7, 9, 12, 5, 8, 10, 15, 20, 35),
                                            Array(7, 4, 11, 12, 7, 5, 18, 10, 17, 30))    
        val y: Array[Int] = Array(0, 1, 0, 0, 0, 1, 0, 0, 1, 1)

        val logit_reg = new Logit(x, y)
        val estimation: DenseVector[Double] = logit_reg.parameters(learning_rate = 0.01, iterations = 1000)

        println(s"The parameters are $estimation")
    }
}


