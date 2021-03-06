package algs

class FibonacciNode[T <% Ordered[T]](var key: T) extends Ordered[FibonacciNode[T]] {
	private[algs] var rank: Int = 0
	private[algs] var parent, firstChild: FibonacciNode[T] = null
	private[algs] var next, prev: FibonacciNode[T] = this
	private[algs] var typ: Int = 2
	
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
	
	def compare(that: FibonacciNode[T]) = this.key.compare(that.key)
}