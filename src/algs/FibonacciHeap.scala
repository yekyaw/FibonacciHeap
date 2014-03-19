package algs

import util.control.Breaks._

class FibonacciHeap[T <% Ordered[T]] {
	private var min: FibonacciNode[T] = null
	private var _size: Int = 0
	
	private def link(n1: FibonacciNode[T], n2: FibonacciNode[T], fair: Boolean): FibonacciNode[T] = {
	  if (n1 == null) return n2
	  if (n2 == null) return n1
	  
	  def linkChild(parent: FibonacciNode[T], child:FibonacciNode[T]): FibonacciNode[T] = {
	    parent.addChild(child) 
	    if (fair) {
	      parent.rank += 1
	      child.typ = 0
	    }
	    else {
	      child.typ = 2
	    }
	    parent
	  }
	  
	  if (n1 < n2) {
	    return linkChild(n1, n2)
	  }  
	  else {  
	    return linkChild(n2, n1)
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
	
	private def cascadingCut(node: FibonacciNode[T]) {
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
	}
	
	def decreaseKey(node: FibonacciNode[T], newKey: T) {
	  if (newKey > node.key) throw new IllegalArgumentException("The new key must be less than the current key!")
	  
	  node.key = newKey
	  if (node ne min) {
	    cascadingCut(node)
	    min = link(min, node, false)
	  }
	}
	
	private def consolidate(parent: FibonacciNode[T]): FibonacciNode[T] = {
	  val log2 = (x: Int) => Math.ceil(Math.log(x) / Math.log(2)).toInt
	  val bins = new Array[FibonacciNode[T]](log2(size) + 1)
	  
	  def addToBin(current: FibonacciNode[T]) {
	    if (current == null) return
	    val next = current.next
	    current.remove()
	    var currentBin = current
	    while (bins(currentBin.rank) != null) {  
	      val bin = bins(currentBin.rank)
	      bins(currentBin.rank) = null
	      currentBin = link(currentBin, bin, true)  
	    }
	    bins(currentBin.rank) = currentBin
	    if (next ne current) addToBin(next)
	  }
	  addToBin(parent.firstChild)
	    
	  var newParent: FibonacciNode[T] = null
	  bins.foreach { bin =>
	    if (bin != null) {
	      if (newParent == null) newParent = bin
	      else newParent = link(newParent, bin, false)
	    }
	  }
	  newParent
	}
	
	def deleteMin(): T = {
	  if (isEmpty()) throw new IllegalStateException("The heap is empty!")
	  
	  val minKey = min.key
	  min = consolidate(min)
	  size -= 1
	  minKey
	}
	
	def delete(node: FibonacciNode[T], minVal: T) {
	  decreaseKey(node, minVal)
	  deleteMin()
	}
	
	def size = _size
	def size_= (value: Int):Unit = _size = value
	
	def isEmpty(): Boolean = {
	  size == 0
	}
	
	def clear() {
	  min = null
	  size = 0
	}
}