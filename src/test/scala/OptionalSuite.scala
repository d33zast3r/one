package com.tkroman.kpi.y2022.l1

import munit.FunSuite
import MyList.*

class MyListSuite extends FunSuite {
  test("split on Nil - 1") {
      val expected = MyNil
      val actual = split(MyNil, 0)
      assertEquals(actual, expected)
  }
  test("split on Cons - 1") {
    val expected = MyList(MyList(1,2), MyList(3,4), MyList(5), MyList(6))
    val actual = split(MyList(1,2,0,3,4,0,5,0,6), 0)
    assertEquals(actual, expected)
  }
  test("split on Cons - 2") {
    val expected = MyList(MyList(21), MyList(2), MyList(5,4), MyNil)
    val actual = split(MyList(21,1,2,1,5,4,1), 1)
    assertEquals(actual, expected)
  }
  test("join on Nil - 1") {
    val expected = MyNil
    val actual = join(MyNil, 0)
    assertEquals(actual, expected)
  }
  test("join on Cons - 1") {
    val expected = MyList(1,2,0,3,4,0,5,0,6)
    val actual = join(MyList(MyList(1,2), MyList(3,4), MyList(5), MyList(6)), 0)
    assertEquals(actual, expected)
  }
  test("join on Cons - 2") {
    val expected = MyList(11,2,11,38,11,11,5,5,5,11,0)
    val actual = join(MyList(MyList(11,2), MyList(38,11), MyList(5,5,5), MyList(0)), 11)
    assertEquals(actual, expected)
  }
  test("replaceWhere on Nil - 1") {
    val expected = MyNil
    val actual = replaceWhere(MyNil, x => None)
    assertEquals(actual, expected)
  }
  test("replaceWhere on Cons - 1") {
    val expected = MyList(2,4,3,4,5)
    val actual = replaceWhere(MyList(1,2,3,4,5), x => if (x < 3) Some(x * 2) else None)
      assertEquals(actual, expected)
  }
  test("replaceWhere on Cons - 2") {
    val expected = MyList(9,10,8,11,7,2,6,3,5,4)
    val actual = replaceWhere(MyList(9,0,8,1,7,2,6,3,5,4), x => if (x < 2) Some(x + 10) else None)
    assertEquals(actual, expected)
  }
  test("dropWhere on Nil - 1") {
    val expected = MyNil
    val actual = dropWhere(MyNil, x => false)
    assertEquals(actual, expected)
  }
  test("dropWhere on Cons - 1") {
    val expected = MyList(1,3,5,7,9)
    val actual = dropWhere(MyList(1,2,3,4,5,6,7,8,9), x => x % 2 == 0)
    assertEquals(actual, expected)
  }
  test("dropWhere on Cons - 2") {
    val expected = MyList(2,4,0,0,8)
    val actual = dropWhere(MyList(1,2,5,4,99,0,0,8,11), x => x % 2 == 1)
    assertEquals(actual, expected)
  }
}