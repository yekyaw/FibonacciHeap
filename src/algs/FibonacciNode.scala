package algs

class FibonacciNode[T <% Ordered[T]](var key: T) {
	var rank: Int = 0
	var parent, firstChild: FibonacciNode[T] = null
	var next, prev: FibonacciNode[T] = this
	var typ: Int = 2
	
	def remove() {
	  assert(parent != null)
	  if (parent.firstChild eq this) {
	    if (next eq this) parent.firstChild = null
	    else parent.firstChild = next
	  }
	  parent = null
	  prev.next = next
	  next.prev = prev
	  next = this
	  prev = this
	}

	def addChild(child: FibonacciNode[T]) {
	  if (firstChild == null) {	
	    firstChild = child
	  }
	  else { 
	    child.next = firstChild 
	    child.prev = firstChild.prev		
	    child.prev.next = child		
	    child.next.prev = child
	  }
	  child.parent = this
	}
}