/*
 Student: Jarin Musarrat
 Program Description: This program is in scala and has a few functions that adds two numbers and subtracts them and the
 program is based on functional and recursion. Ii doesn't use any loop or library.
 */

abstract class MyList
{
  def head: Int
  def tail: MyList
  def isEmpty: Boolean
  def addElem(elem: Int): MyList
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}
object Empty extends MyList
{
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def addElem(elem: Int): MyList = new Cons(elem, Empty)
  def printElements: String = ""
}

class Cons(h: Int, t: MyList) extends MyList {
  def head: Int  = h
  def tail: MyList = t
  def isEmpty: Boolean = false
  def addElem(elem: Int): MyList = new Cons(elem, this)
  def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
}
object hw7 extends App {

  //addition function
  def addList(l1: MyList, l2: MyList, c: Int = 0): MyList = {

    if ((l1.isEmpty && l2.isEmpty) && c==0)
       Empty
    else if (l1.isEmpty && c==0)
      new Cons(l2.head,Empty)
    else if (l2.isEmpty && c==0)
      new Cons(l1.head,Empty)
    else if ((l1.isEmpty && l2.isEmpty) && c==1)
      new Cons(1,Empty)
    else if (l1.isEmpty && c==1)
      new Cons (l2.head+c,Empty)
    else if (l2.isEmpty && c==1)
      new Cons (l1.head+c, Empty)
    else if (l1.head+l2.head+c>9) {
      new Cons((l1.head+l2.head+c)-10,addList(l1.tail,l2.tail,1))
    } else
      new Cons(l1.head+l2.head+c, addList(l1.tail,l2.tail,0))
  }

  // create list function
  def createList(num: Int): MyList = {
    if (num>0){
      val r = num%10
      val temp = num/10
      new Cons (r,createList(temp))
    }else {
      Empty
    }
  }

  //getting int value from list element function
  def intValue(l: MyList): Int = {

      times(l,1)
  }
  //this times function helps to get int value from the int value function
  def times(l:MyList,n:Int):Int ={

    if (!l.isEmpty){
      times(l.tail,10*n)+(l.head*n)
    }else {
      0
    }

  }

  //subtraction function
  def subList(l1: MyList, l2: MyList, c: Int = 0): MyList = {
    //this part handles if the 2nd list is longer
    if ((l1.isEmpty && l2.isEmpty) && c==0)
      Empty
    else if ((l1.isEmpty && l2.isEmpty) && c==1)
      new Cons (-1,Empty)
    else if (l1.isEmpty && c==0)
      new Cons ( -l2.head, subList(Empty,l2.tail,0))
    else if (l1.isEmpty && c==1 && l2.head>=1)
      new Cons ( l2.head-1, Empty)
    else if (l1.isEmpty && c==1 && l2.head<1)
      new Cons ( (l2.head+10)-1, subList(Empty,l2.tail,1))
    else if (l1.isEmpty && l2.head+c>0)
      new Cons (10-(l2.head+c),subList(Empty,l2.tail,1))
    else if (l1.isEmpty && l2.head+c<=0)
      new Cons (0-(l2.head+c),subList(Empty,l2.tail,1))

 // this part handles if the first list is longer
    else if (l2.isEmpty && c==0)
      new Cons ( l1.head, subList(l1.tail,Empty,0))
    else if (l2.isEmpty && c==1 && l1.head>=1)
      new Cons ( l1.head-1, Empty)
    else if (l2.isEmpty && c==1 && l1.head<1)
      new Cons ( (l1.head+10)-1, subList(l1.tail,Empty,1))
    else if (l1.head<l2.head+c)
      new Cons ((l1.head+10)-(l2.head+c),subList(l1.tail,l2.tail,1))
    else
      new Cons(l1.head-(l2.head+c), subList(l1.tail,l2.tail,0))
  }

  println("--prob 1--")
  val list1 = new Cons(3, new Cons(4, new Cons(5, Empty)))
  val list2 = new Cons(1, new Cons(2, new Cons(3, Empty)))
  println(list1)
  println(list2)
  println(addList(list1, list2))
  println()

  println("--prob 2--")
  val list12 = new Cons(8, new Cons(7, new Cons(2, Empty)))
  val list13 = new Cons(8, new Cons(8, new Cons(2, Empty)))
  val list14 = new Cons(8, new Cons(7, new Cons(9, Empty)))

  println(addList(list1, list12))
  println(addList(list13, list2))
  println(addList(list14, list1))

  println()

    println("--prob 3: Bonus--")
    val list3 = new Cons(1, new Cons(2, new Cons(3, new Cons(4, Empty))))
    println(addList(list1, list3))
    println(addList(list3, list2))
    println(addList(list3, list14))
    println()

    println("--prob 4--")
    println(intValue(list3))
    println(intValue(list1))
    println()

    println("--prob 5--")
    val list4 = createList(321)
    println(list4)
    println(list1)
    println()

    println("--prob 6--")
    println(subList(list1, list2))
    val list5:MyList = createList(657)//657
    val list6 = createList(334)
    val list7 = subList(list5, list6)
    println(list7)
    println()

   println("--prob 7--")
   var l1 = createList(523)
   var l2 = createList(325)
   var l3 = subList(l1, l2)
   println(intValue(l3))
   l1 = createList(746)
   l2 = createList(359)
   l3 = subList(l1, l2)
   println(l3)
   println(intValue(l3))

   println("--prob 8--")
   val l5 = createList(523)
   val l6 = createList(625)
   val l7 = createList(1021)//1021

   val l8 = subList(l5, l6)
   val l9 = subList(l5, l7)
   println(intValue(l8))
   println(intValue(l9))

   println(subList(l7, l5))
   println(subList(l7, l6))

}
