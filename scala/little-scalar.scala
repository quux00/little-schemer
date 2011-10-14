//package little.scalar

trait LittleScalar {

  def atom_?(x: Any): Boolean = ! x.isInstanceOf[Seq[_]]
  
  def lat_?(l: Seq[_]): Boolean = {
    if      (l.isEmpty) true
    else if ( ! atom_?(l.head) ) false
    else    lat_?(l.tail)
  }

  def member_?(a: Any, lat: Seq[_]): Boolean = {
    if   (lat.isEmpty) false
    else (a == lat.head) || member_?(a, lat.tail)
  }

  def rember(a: Any, lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (a == lat.head) lat.tail
    // else lat.head + rember(a, lat.tail)
    else lat.head +: rember(a, lat.tail)
  }

  def firsts(l: Seq[Seq[_]]): Seq[_] = {
     if (l.isEmpty) l
     else l.head.head +: firsts(l.tail)
  }

  def insertR(nw: Any, old: Any, lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (old == lat.head) old +: (nw +: lat.tail)
    // else if (old == lat.head) Seq(old, nw) +: insertR(lat.tail)
    else lat.head +: insertR(nw, old, lat.tail)
  }

  def insertL(nw: Any, old: Any, lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (old == lat.head) nw +: (old +: lat.tail)
    else lat.head +: insertL(nw, old, lat.tail)
  }

  def subst(nw: Any, old: Any, lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (old == lat.head) nw +: lat.tail
    else lat.head +: subst(nw, old, lat.tail)
  }

  def subst2(nw: Any, o1: Any, o2: Any, lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (o1 == lat.head || o2 == lat.head) nw +: lat.tail
    else lat.head +: subst2(nw, o1, o2, lat.tail)
  }

  def multirember(a: Any, lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (a == lat.head) multirember(a, lat.tail)
    else lat.head +: multirember(a, lat.tail)
  }

  def multiinsertR(nw: Any, old: Any, lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (old == lat.head) old +: (nw +: multiinsertR(nw, old, lat.tail))
    else lat.head +: multiinsertR(nw, old, lat.tail)
  }

  def multiinsertL(nw: Any, old: Any, lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (old == lat.head) nw +: (old +: multiinsertL(nw, old, lat.tail))
    else lat.head +: multiinsertL(nw, old, lat.tail)
  }

  def multisubst(nw: Any, old: Any, lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (old == lat.head) nw +: multisubst(nw, old, lat.tail)
    else lat.head +: multisubst(nw, old, lat.tail)
  }

  /* ---[ Chapter 4 ]--- */
  // TODO: not sure how to make this generic to all numeric types in Scala,
  // since there isn't a single superclass for all numeric types (AnyVal is
  // also a trait for Boolean and Unit, so doesn't have numeric operators
  // def addNum[T](n: Numeric[T], m: Numeric[T]): Numeric[T] = {
  //   if (m == 0) n
  //   else 1 + (addNum(n, m-1))  //~TODO: this fails bcs Numeric does not have a "-" operator
  // }
  
  def add(n: Int, m: Int): Int = {
    if (m == 0) n
    else 1 + (add(n, m-1))
  }

  def subtract(n: Int, m: Int): Int = {
    if (m == 0) n
    else (-1) + (subtract(n, (m-1)))
  }
  
  def addtup(tup: Seq[Int]): Int = {
    if (tup.isEmpty) 0
    else add(tup.head, addtup(tup.tail))
  }

  def mult(n: Int, m: Int): Int = {
    if (m == 0 || n == 0) 0
    else if (m == 1) n
    else n + mult(n, (m-1))
  }

  def tup_+(tup1: Seq[Int], tup2: Seq[Int]): Seq[Int] = {
    if      (tup1.isEmpty) tup2
    else if (tup2.isEmpty) tup1
    else (tup1.head + tup2.head) +: tup_+(tup1.tail, tup2.tail)
  }

  def exp(n: Int, m: Int): Int = {
    if (m == 0) 1
    else n * exp(n, (m-1))
  }

  def quotient(n: Int, m: Int): Int = {
    if (n < m) 0
    else 1 + quotient(n-m, m)
  }

  def length(lat: Seq[_]): Int = {
    if (lat.isEmpty) 0
    else 1 + length(lat.tail)
  }
}

object L extends LittleScalar


/* ---- ---------------------------------------  --- */
/* ---[ implementation tests and assert methods ]--- */
/* ---  ---------------------------------------  --- */

def assertT(b: Boolean, msg: String = "") = {
  if (b) print(".")
  else printf("F => %s", msg)
}

def assertF(b: Boolean, msg: String = "") = assertT(!b, msg)

def assertEq(a: Any, b: Any, msg: String = "") = {
  if (a == b) print(".")
  else printf("F => %s; exp: %s; act: %s\n", msg, a, b)
}

def header(title: String) = printf("\n========= %s =========\n", title)

val lat = List(1,2,3)
val ll  = List(1,2,lat)

header("atom? and lat?")
assertT( L.atom_?(1),   "1" )
assertF( L.atom_?(lat), "2" )
assertT( L.lat_?(lat),  "3" )
assertF( L.lat_?(ll),   "4")

header("member?")
assertT( L.member_?(1, lat),     "1" )
assertT( L.member_?(3, lat),     "2" )
assertF( L.member_?(44, lat),    "3" )
assertT( L.member_?(2, ll),      "4" )
assertT( L.member_?("cat", List("dog", "frog", "cat", "mouse")),      "5" )
assertF( L.member_?("moose", List("dog", "frog", "cat", "mouse")),      "6" )

header("rember")
assertEq( List(1,2), L.rember(3, List(1,2,3)), "1")
assertEq( List(1,3), L.rember(2, List(1,2,3)), "2")

header("firsts")
val fl1 = List( List(1,2,3), List(4,5,6) )
val fl2 = List( List("a",1,2,3), List(4,5,6), List("b", "c"), List("z") )
val fl3 = List( List(1,2) );
assertEq( List(1,4), L.firsts(fl1), "1")
assertEq( List("a",4,"b","z"), L.firsts(fl2), "2")
assertEq( List(1), L.firsts(fl3), "3")

header("insertR")
assertEq( List(1,2,3,666,4,"5"), L.insertR(666, 3, List(1,2,3,4,"5")), "1")
assertEq( List(1,666,2,3,4,"5"), L.insertR(666, 1, List(1,2,3,4,"5")), "2")
assertEq( List(1,2,3,4,"5"), L.insertR(666, "NA", List(1,2,3,4,"5")), "3")
assertEq( List(), L.insertR("new", "old", List()), "5")
assertEq( List(1,2), L.insertR("new", null, List(1,2)), "6")

header("insertL")
assertEq( List(1,2,666,3,4,"5"), L.insertL(666, 3, List(1,2,3,4,"5")), "1")
assertEq( List(666,1,2,3,4,"5"), L.insertL(666, 1, List(1,2,3,4,"5")), "2")
assertEq( List(1,2,3,4,"5"), L.insertL(666, "NA", List(1,2,3,4,"5")), "3")
assertEq( List(), L.insertL("new", "old", List()), "5")
assertEq( List(1,2), L.insertL("new", null, List(1,2)), "6")

header("subst")
assertEq( List(1,2,666,4,"5"), L.subst(666, 3, List(1,2,3,4,"5")), "1")
assertEq( List(666,2,3,4,"5"), L.subst(666, 1, List(1,2,3,4,"5")), "2")
assertEq( List(1,2,3,4,"5"), L.subst(666, "NA", List(1,2,3,4,"5")), "3")
assertEq( List(), L.subst("new", "old", List()), "5")
assertEq( List(1,2), L.subst("new", null, List(1,2)), "6")

header("subst2")
assertEq( List(1,2,666,4,"5"), L.subst2(666, 3, 4, List(1,2,3,4,"5")), "1")
assertEq( List(666,2,3,4,"5"), L.subst2(666, 1, 4, List(1,2,3,4,"5")), "2")
assertEq( List(666,1,2,3,4), L.subst2(666, 1, 4, List(4,1,2,3,4)), "2")
assertEq( List(1,2,3,4,"5"), L.subst2(666, "NA1", "NA2", List(1,2,3,4,"5")), "3")
assertEq( List(), L.subst2("new", "o1", "o2", List()), "5")
assertEq( List(1,2), L.subst2("new", null, null, List(1,2)), "6")

header("multirember")
assertEq( List("a"), L.multirember("ab",List("a", "ab")), "1")
assertEq( List(),    L.multirember("ab", List()), "2")
assertEq( List("a","b","c"), L.multirember("x", List("a","b","c")), "3")
assertEq( List("a","c"),  L.multirember("ab", List("a","ab","c")), "4")
assertEq( List("a","c"),  L.multirember("ab", List("a","ab","ab","c")), "5")
assertEq( List("a","c"), 
         L.multirember("ab", List("ab","a","ab","c","ab","ab")), "6")
assertEq( List("x"), 
         L.multirember("ab", List("ab","ab","ab","ab","x")), "7")
assertEq( List(), 
         L.multirember("ab", List("ab","ab","ab","ab")), "8")

header("multiinsertR")
assertEq( List("a","ab",1), L.multiinsertR(1,"ab",List("a", "ab")), "1")
assertEq( List("a","ab",1,"ab",1), 
         L.multiinsertR(1,"ab",List("a","ab","ab")), "2")
assertEq( List("a","ab",1,"ab",1,"b","ab",1,"c"), 
         L.multiinsertR(1,"ab",List("a","ab","ab","b","ab","c")), "3")
assertEq( List(), L.multiinsertR("new", "old", List()), "4")
assertEq( List(1,2), L.multiinsertR("new", "old", List(1,2)), "5")


header("multiinsertL")
assertEq( List("a",1,"ab"), L.multiinsertL(1,"ab",List("a", "ab")), "1")
assertEq( List("a",1,"ab",1,"ab"), 
         L.multiinsertL(1,"ab",List("a","ab","ab")), "2")
assertEq( List("a",1,"ab",1,"ab","b",1,"ab","c"), 
         L.multiinsertL(1,"ab",List("a","ab","ab","b","ab","c")), "3")
assertEq( List(), L.multiinsertL("new", "old", List()), "4")
assertEq( List(1,2), L.multiinsertL("new", "old", List(1,2)), "5")

header("multisubst")
assertEq( List("a",1), L.multisubst(1,"ab",List("a", "ab")), "1")
assertEq( List("a",1,1), 
         L.multisubst(1,"ab",List("a","ab","ab")), "2")
assertEq( List("a",1,1,"b",1,"c"), 
         L.multisubst(1,"ab",List("a","ab","ab","b","ab","c")), "3")
assertEq( List(), L.multisubst("new", "old", List()), "4")
assertEq( List(1,2), L.multisubst("new", "old", List(1,2)), "5")

header("add")
assertEq( 7, L.add(4,3), "1" )
assertEq( 7, L.add(0,7), "2" )
assertEq( 7, L.add(7,0), "3" )

header("subtract")
assertEq( 1, L.subtract(4,3),  "1")
assertEq( 4, L.subtract(7,3),  "2")
assertEq( 4, L.subtract(4,0),  "3")

// tuple is defined as a list of number (Ints here)
header("addtup")
assertEq( 8, L.addtup(List(1,4,3)), "1" )
assertEq( 7, L.addtup(List(0,7)), "2" )
assertEq( 17, L.addtup(List(0,7,3,6,1)), "2" )
assertEq( 1, L.addtup(List(1)), "2" )

header("mult")
assertEq( 12, L.mult(4,3),  "1")
assertEq( 21, L.mult(7,3),  "2")
assertEq( 0,  L.mult(4,0),  "3")

header("tup_+")
val tupl = List(1,2,3,4)
assertEq( List(2,4,6,8), L.tup_+(tupl, tupl), "1")
assertEq( List(2,4,6,4), L.tup_+(tupl, List(1,2,3)), "2")
assertEq( List(2,4,6,4), L.tup_+(List(1,2,3), tupl), "3")
assertEq( List(2,2,3,4), L.tup_+(List(1), tupl), "3")

header("exp")
assertEq( 16, L.exp(2,4),  "1")
assertEq( 1, L.exp(1,3),  "2")
assertEq( 1,  L.exp(4,0),  "3")
assertEq( 256,  L.exp(2,8),  "4")

header("quotient")
assertEq( 2, L.quotient(4,2),  "1")
assertEq( 3, L.quotient(3,1),  "2")
assertEq( 2,  L.quotient(11,5),  "3")
assertEq( 11,  L.quotient(82,7),  "4")

