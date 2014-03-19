package algs

import scala.util.Random

object TestHeap {
	def main(args: Array[String]) {
	  val currentTime = System.currentTimeMillis()
	  val tree = new FibonacciHeap[Int]()
	  val numIterations = 1000000
	  1 to numIterations foreach { i =>
	    val node = new FibonacciNode(Random.nextInt())
	    tree.insert(node)
	    if (i % (numIterations / 100) == 0) {
	      tree.deleteMin()
	    }
	    else if (i % 100 == 0) {
	      tree.decreaseKey(node, node.key - 100)
	    }
	  }
	  var previous = tree.deleteMin()
	  while (!tree.isEmpty()) {
	    val min = tree.deleteMin()
	    assert(min >= previous)
	    previous = min
	  }
	  println(System.currentTimeMillis() - currentTime)
	}
}