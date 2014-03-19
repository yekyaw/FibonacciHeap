package algs

import scala.util.Random

object TestHeap {
	def main(args: Array[String]) {
	  val currentTime = System.currentTimeMillis()
	  val tree = new Tree[Int]()
	  val numIterations = 100000
	  1 to numIterations foreach { i =>
	    val node = new Node(Random.nextInt())
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
	    assert(min > previous)
	  }
	  println(System.currentTimeMillis() - currentTime)
	}
}