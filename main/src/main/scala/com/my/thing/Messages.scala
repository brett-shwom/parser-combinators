package com.my.thing

case class A(f1 : String, f2 : Seq[Int], f3 : B, f4 : Int = 0, f5: Option[String]=None)
case class B(f1 : String)