package com.my.thing

case class A(f1 : String, f2 : Seq[Int], f3 : B, f4 : Int = 0, f5: Option[String]=None)
case class B(f1 : String, f2 : Option[C] = None)
case class C(f1 : String)

case class Simple(f1 : Option[String])
case class LessSimple(f1 : Option[String], f2 : Int) 