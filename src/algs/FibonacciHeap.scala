package algs

import util.control.Breaks._

class FibonacciHeap[T <% Ordered[T]] {
	var min: FibonacciNode[T] = null
	var size: Int = 0
	
	private def link(n1: FibonacciNode[T], n2: FibonacciNode[T], fair: Boolean): FibonacciNode[T] = {
	  if (n1 == null) return n2
	  if (n2 == null) return n1
	  if (n1.key < n2.key) {
	    n1.addChild(n2) 
	    if (fair) {
	      n1.rank += 1
	      n2.typ = 0
	    }
	    else n2.typ = 2
	    return n1
	  }  
	  else {  
	    n2.addChild(n1)  
	    if (fair) {
	      n2.rank += 1
	      n1.typ = 0
	    }
	    else n1.typ = 2
	    return n2
	  }
	}
	
	def insert(node: FibonacciNode[T]) {
	  min = link(min, node, false)
	  size += 1
	}
	
	def findMin(): T = {
	  if (isEmpty()) throw new IllegalStateException("The heap is empty!")
	  min.key
	}
	
	def meld(that: FibonacciHeap[T]) {
	  min = link(min, that.min, false)
	  size += that.size
	}
	
	def decreaseKey(node: FibonacciNode[T], newKey: T) {
	  if (newKey > node.key) throw new IllegalArgumentException("The new key must be less than the current key!")
	  
	  node.key = newKey
	  if (node ne min) {
	    var current = node.parent
	    while ((current.parent != null) && (current.typ == 1)) {
	      current.rank -= 1
	      current.typ = 2
	      current = current.parent
	    }
	    if (current ne min) current.rank -= 1
	    if ((current.parent != null) && (current.typ == 0)) {
	      current.typ = 1
	    }
	    node.remove()
	    min = link(min, node, false)
	  }
	}
	
	def deleteMin(): T = {
	  if (isEmpty()) throw new IllegalStateException("The heap is empty!")
	  
	  val minKey = min.key
	  
	  val log2 = (x: Int) => Math.ceil(Math.log(x) / Math.log(2)).toInt
	  val bins = new Array[FibonacciNode[T]](log2(size) + 1)
	  var current = min.firstChild
	  breakable {
	    while (current != null) {
	      val next = current.next
	      current.remove()
	      var currentBin = current
	      while (bins(currentBin.rank) != null) {
	        val bin = bins(currentBin.rank)
	        bins(currentBin.rank) = null
	        currentBin = link(currentBin, bin, true)
	      }
	      bins(currentBin.rank) = currentBin
	      if (current eq next) break
	      current = next
	    }
	  }
	    
	  min = null
	  bins.foreach { bin =>
	    if (bin != null) {
	      if (min == null) min = bin
	      else min = link(min, bin, false)
	    }
	  }
	  size -= 1
	  minKey
	}
	
	def delete(node: FibonacciNode[T], minVal: T) {
	  decreaseKey(node, minVal)
	  deleteMin()
	}
	
	def isEmpty(): Boolean = {
	  size == 0
	}
}