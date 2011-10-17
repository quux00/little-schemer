//package little.scalar

trait LittleScalar {

  // def atom_?(x: Any): Boolean = ! x.isInstanceOf[Seq[_]]
  
  def atom_?(x: Any): Boolean = {
    x match {
      case x: Seq[_] => false
      case (x,y,z)   => false
      case _         => true
    }
  }  

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

  def pick[T](n: Int, lat: Seq[T]): T = {
    if (lat.isEmpty) throw new RuntimeException("Empty list")
    else if (n == 1) lat.head
    else pick(n-1, lat.tail)
  }
  
  // so by returning "Any" you can return null
  // but by returning a Type param T, you cannot return null
  // (in the "pick" version) - not sure why ...
  def pick2(n: Int, lat: Seq[_]): Any = {
    if (lat.isEmpty) null
    else if (n == 1) lat.head
    else pick2(n-1, lat.tail)
  }

  // here I try to address with Scala's Options - 
  // still not sure this is the right idiomatic way
  // to do it in Scala, but probably better than
  // first two impl's
  def pick3[T](n: Int, lat: Seq[T]): Option[T] = {
    if (lat.isEmpty) None
    else if (n == 1) Some(lat.head)
    else pick3(n-1, lat.tail)
  }
  
  def rempick(n: Int, lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (n == 1) lat.tail
    else lat.head +: rempick(n-1, lat.tail)
  }

  def number_?(x: Any) = {
    // there is probably a better way to do this in Scala, but
    // after an hour of researching it I couldn't figure it out
    x.isInstanceOf[Int] || x.isInstanceOf[Double]
  }

  def noNums(lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (number_?(lat.head)) noNums(lat.tail)
    else lat.head +: noNums(lat.tail)
  }
  
  def allNums(lat: Seq[_]): Seq[_] = {
    if (lat.isEmpty) lat
    else if (number_?(lat.head)) lat.head +: allNums(lat.tail)
    else allNums(lat.tail)
  }
  
  def occur(a: Any, lat: Seq[_]): Int = {
    if (lat.isEmpty) 0
    else if (a == lat.head) 1 + occur(a, lat.tail)
    else occur(a, lat.tail)
  }
  
  /* ---[ Chapter 5 ]--- */
  def rember_*(a: Any, l: Seq[_]): Seq[_] = {
    if (l.isEmpty) {
      l
    } else if (atom_?(l.head)) {
      if (l.head == a) rember_*(a, l.tail)
      else l.head +: rember_*(a, l.tail)
      
    } else {
      rember_*(a, l.head.asInstanceOf[Seq[_]]) +: rember_*(a, l.tail)
    }
  }
  
  def insertR_*(nw: Any, old: Any, l: Seq[_]): Seq[_] = {
    if (l.isEmpty) {
      l
    } else if (atom_?(l.head)) {
      if (l.head == old) old +: (nw +: insertR_*(nw, old, l.tail))
      else l.head +: insertR_*(nw, old, l.tail)
      
    } else {
      insertR_*(nw, old, l.head.asInstanceOf[Seq[_]]) +:
      insertR_*(nw, old, l.tail)
    }
  }
  
  def insertL_*(nw: Any, old: Any, l: Seq[_]): Seq[_] = {
    if (l.isEmpty) {
      l
    } else if (atom_?(l.head)) {
      if (l.head == old) nw +: (old +: insertL_*(nw, old, l.tail))
      else l.head +: insertL_*(nw, old, l.tail)
      
    } else {
      insertL_*(nw, old, l.head.asInstanceOf[Seq[_]]) +:
      insertL_*(nw, old, l.tail)
    }
  }

  def occur_*(a: Any, l: Seq[_]): Int = {
    if (l.isEmpty) {
      0
    } else if (atom_?(l.head)) {
      if (l.head == a) 1 + occur_*(a, l.tail)
      else occur_*(a, l.tail)
    } else {
      occur_*(a, l.head.asInstanceOf[Seq[_]]) + 
      occur_*(a, l.tail)
    }
  }

  def subst_*(nw: Any, old: Any, l: Seq[_]): Seq[_] = {
    if (l.isEmpty) l
    else if (atom_?(l.head)) {
      if (l.head == old) nw +: subst_*(nw, old, l.tail)
      else l.head +: subst_*(nw, old, l.tail)
    } else {
      subst_*(nw, old, l.head.asInstanceOf[Seq[_]]) +: subst_*(nw, old, l.tail)
    }
  }

  def member_?*(a: Any, l: Seq[_]): Boolean = {
    if (l.isEmpty) false
    else if (atom_?(l.head)) {
      if (l.head == a) true
      else member_?*(a, l.tail)
    } else {
      member_?*(a, l.head.asInstanceOf[Seq[_]]) ||
      member_?*(a, l.tail)
    }
  }

  def leftmost_*(l: Seq[_]): Any = {
    if (l.isEmpty) null
    else if (atom_?(l.head)) l.head
    else leftmost_*(l.head.asInstanceOf[Seq[_]])
  }

  def eqlist_?(l1: Seq[_], l2: Seq[_]): Boolean = {
    if (l1.isEmpty || l2.isEmpty) {
      (l1.isEmpty && l2.isEmpty)
    } else if (atom_?(l1.head) || atom_?(l2.head)) {
      if (atom_?(l1.head) && atom_?(l2.head) &&
          l1.head == l2.head) {
        eqlist_?(l1.tail, l2.tail)
      } else {
        false
      }
    } else {
      (eqlist_?(l1.head.asInstanceOf[Seq[_]], 
                l2.head.asInstanceOf[Seq[_]])
       &&
       eqlist_?(l1.tail, l2.tail))
    }
  }

  /* ---[ Chapter 6 ]--- */

  // helper method I added not from the Little Schemer
  def arithExpr_?(exp: Any): Boolean = {
    import scala.util.matching.Regex
    exp match {
      // check if it is Tuple3[_,String,_] and that String
      // is a valid operator
      case (x, y: String, z) => 
        ("[-+*/^]".r findFirstIn y) != None
      case _ => false
    }
  }
  
  def infixArithExpr_?(exp: Any): Boolean = {
    import scala.util.matching.Regex
    def isValidOp(s: String): Boolean = 
      ("[-+*/^]".r findFirstIn s) != None

    exp match {
      // ensure this is a Tuple3 of Int-String-Int
      case (x: Int, y: String, z: Int)       => isValidOp(y)
      case (x: Int, y: String, z: Double)    => isValidOp(y)
      case (x: Double, y: String, z: Int)    => isValidOp(y)
      case (x: Double, y: String, z: Double) => isValidOp(y)
      case _ => false
    }
  }
  
  // Because of the type system in Scala, the dynamic
  // version in the Clojure code is nearly unworkably
  // clunky, so I've changed the model.
  // This one is more idiomatic Scala (I think)
  // and uses Tuple3 for the infix-notation
  // arithmetic expression: (1, "*", 2)
  // rather than List(1, "*", 2)
  def numbered_?(aexp: Any): Boolean = {
    if (atom_?(aexp)) number_?(aexp)
    else if (infixArithExpr_?(aexp)) true
    else (arithExpr_?(aexp) &&
          numbered_?(aexp.asInstanceOf[Tuple3[_,_,_]]._1) &&
          numbered_?(aexp.asInstanceOf[Tuple3[_,_,_]]._3))
  }
  
  def value(nexp: Any): Double = {
    if (number_?(nexp)) nexp.asInstanceOf[Int]

    else if ( ! numbered_?(nexp) ) { 
      throw new IllegalArgumentException("Argument is not an infix numbered expression")

    } else {
      val exp1 = nexp.asInstanceOf[Tuple3[_,_,_]]._1
      val op: String = nexp.asInstanceOf[Tuple3[_,_,_]]._2.asInstanceOf[String]
      val exp3 = nexp.asInstanceOf[Tuple3[_,_,_]]._3
      if      (op == "+") value(exp1) + value(exp3)
      else if (op == "*") value(exp1) * value(exp3)
      else if (op == "-") value(exp1) - value(exp3)
      else if (op == "/") value(exp1) / value(exp3)
      else if (op == "^") math.pow(value(exp1), value(exp3))
      else throw new IllegalArgumentException( 
        String.format("Operator '%s' is not recognized", op))
    }
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

header("pick")
val pl = List(1,2,3,4,5)
assertEq(1, L.pick(1, pl), "1")
assertEq(3, L.pick(3, pl), "2")
assertEq(4, L.pick(4, pl), "3")
var exception = false
try {
  L.pick(8, pl)
} catch {
  case ex: RuntimeException => exception = true
} finally {
  assertT(exception, "4")
}

header("pick2")
assertEq(1, L.pick2(1, pl), "1")
assertEq(3, L.pick2(3, pl), "2")
assertEq(4, L.pick2(4, pl), "3")
assertEq(null, L.pick2(8, pl), "4")

header("pick3")
assertF( L.pick3(1, pl).isEmpty, "1-1")
assertEq(1, L.pick3(1, pl).get, "1-2")
assertEq(3, L.pick3(3, pl).get, "2")
assertEq(4, L.pick3(4, pl).get, "3")
assertT( L.pick3(8, pl).isEmpty, "4-1")
assertEq(None, L.pick3(8, pl), "4-2")

header("rempick")
assertEq(List(2,3,4,5), L.rempick(1, pl),     "1")
assertEq(List(1,3,4,5), L.rempick(2, pl),     "2")
assertEq(List(1,2,3,4), L.rempick(5, pl),     "3")
assertEq(pl,            L.rempick(9, pl),     "4")
assertEq(List(),        L.rempick(1, List()), "5")

header("noNums")
assertEq(List(),         L.noNums( List(1,2,3) ),         "1")
assertEq(List("a", "b"), L.noNums( List(1,"a",2,3,"b") ), "2")
assertEq(List("a", "b"), L.noNums( List("a","b") ),       "3")

header("allNums")
assertEq(List(1,2,3), L.allNums( List(1,2,3) ),         "1")
assertEq(List(1,2,3), L.allNums( List(1,"a",2,3,"b") ), "2")
assertEq(List(),      L.allNums( List("a","b") ),       "3")

header("occur")
assertEq( 0, L.occur(1, List()), "1")
assertEq( 0, L.occur(1, List("a", "b", 33)), "2")
assertEq( 1, L.occur("a", List("a", "b", 33)), "3")
assertEq( 3, L.occur("a", List("a", "b", 33, 1, "a", "a", "ab")), "4")

header("rember_*")
assertEq( List(1,3,4), L.rember_*(2, List(1,2,3,4,2)), "1")
assertEq( List(List(1),List(3,4)), 
         L.rember_*(2, List(List(1,2),List(3,4))), "2")
assertEq( List(1,List(1),List(3,4),3,List(1,4,44)), 
         L.rember_*(2, List(1,List(1,2),List(3,4),2,3,List(1,4,44))), "3")
assertEq( List( List(), List(), 3 ),
         L.rember_*(6, List(6, List(6,6), List(6), 3)), "4")


header("insertR_*")
assertEq( List(10,11), L.insertR_*(11, 10, List(10)), "1")
assertEq( List(10), L.insertR_*(10, 11, List(10)), "2")
assertEq( List(1,2,2,3,4,2,2), L.insertR_*(2, 2, List(1,2,3,4,2)), "3")
assertEq( List(List(1,2,"b"),List(3,4)), 
         L.insertR_*("b", 2, List(List(1,2),List(3,4))), "4")

assertEq( List(1,List(2,"a",1),List(3,4),2,"a",3,List(1,4,44)), 
         L.insertR_*("a", 2, 
                     List(1,List(2,1),List(3,4),2,3,List(1,4,44))), "5")
assertEq( List(6, List(6,6), List(6), 3),
         L.insertR_*(6, "NA", List(6, List(6,6), List(6), 3)), "6")

header("insertL_*")
assertEq( List(11,10), L.insertL_*(11, 10, List(10)), "1")
assertEq( List(10), L.insertL_*(10, 11, List(10)), "2")
assertEq( List(1,2,2,3,4,2,2), L.insertL_*(2, 2, List(1,2,3,4,2)), "3")
assertEq( List(List(1,"b",2),List(3,4)), 
         L.insertL_*("b", 2, List(List(1,2),List(3,4))), "4")

assertEq( List(1,List("a",2,1),List(3,4),"a",2,3,List(1,4,44)), 
         L.insertL_*("a", 2, 
                     List(1,List(2,1),List(3,4),2,3,List(1,4,44))), "5")
assertEq( List(6, List(6,6), List(6), 3),
         L.insertL_*(6, "NA", List(6, List(6,6), List(6), 3)), "6")

header("occur_*")
assertEq( 1, L.occur_*( 8, List(7,8,1)), "1")
assertEq( 2, L.occur_*( 8, List(7,8,List(8))), "2")
assertEq( 0, L.occur_*( 1, List(7,8,List(8))), "3")
assertEq( 2, L.occur_*( 8, List(7,8,List(1, List(3,8,2)), 1)), "4")
assertEq( 0, L.occur_*( 0, List()), "5")

header("subst_*")
assertEq( List(7,8,"a"), L.subst_*( "a", 1, List(7,8,1)), "1")
assertEq( List(7,"a",List("a")), 
         L.subst_*( "a", 8, List(7,8,List(8))), "2")
assertEq( List(7,8,List(8)), L.subst_*( "a", 1, List(7,8,List(8))), "3")
assertEq( List(7,"a",List(1, List(3,"a",2)), 1), 
         L.subst_*( "a", 8, List(7,8,List(1, List(3,8,2)), 1)), "4")
assertEq( List(), L.subst_*( "a", 0, List()), "5")

header("member_?*")
assertT( L.member_?*(1, List(1,2,3)), "1" )
assertF( L.member_?*(5, List(1,2,3)), "2" )
assertT( L.member_?*(5, List(1,2,3,List(1,5,6))), "3" )
assertF( L.member_?*("s", List(1,2,3,List(1,5,6))), "4" )

header("leftmost_*")
assertEq( 1,   L.leftmost_*(List(1,2,3)), "1")
assertEq( 1,   L.leftmost_*(List(List(1,2),1,2,3)), "2")
val listAB = List("a","b")
val listMid = List(listAB, 1,2)
val listOuter = List(listMid, 3,4)
assertEq( "a", L.leftmost_*(listOuter), "3")


header("eqlist_?")
val listEF = List("e", "f")
assertT( L.eqlist_?(listEF, listEF), "1")
assertF( L.eqlist_?(listEF, listAB), "2")
assertT( L.eqlist_?(listOuter, listOuter), "3")
assertF( L.eqlist_?(listOuter, listMid), "4")
assertF( L.eqlist_?( List(1,2,3,4), List(1,2,3) ), "5")
assertF( L.eqlist_?( List(1,2, List("a")),
                    List(1,2, List("b"))), "6")
val longList1 = List(1,List(2,List(List("a"))),"b",List("c","d"), List("e", List(11)))
val longList2 = List(1,List(2,List(List("a"))),"b",List("c","d"), List("e", List(12)))
assertF( L.eqlist_?(longList1, longList2), "7")


header("numbered_?")
assertT( L.numbered_?(1),              "1")
assertF( L.numbered_?("*"),            "2")
assertT( L.numbered_?( (1, "*", 2) ),  "3")
assertT( L.numbered_?( (1, "^", 2) ),  "4")
assertF( L.numbered_?( (1, 44, 2) ),   "5")
assertF( L.numbered_?( (1, "NA", 2) ), "6")

header("value")
assertEq( 1, L.value(1),    "1")
assertEq( 2, L.value( (1,"+",1) ),  "2")
assertEq( 4, L.value( ((1,"*",2),"+",2) ),  "3")
assertEq( 22, L.value( ((1, "*", 2), "+", (2, "*", 10)) ), "4")
assertEq( 408, L.value( (((1,"*",2), "^", 3), "+", 
                         ((2,"*",10), "^", 2)) ), "5")
