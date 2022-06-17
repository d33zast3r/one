package com.tkroman.kpi.y2022.l1

import scala.annotation.tailrec
import scala.collection.mutable

enum MyList[+A]:
  case MyNil
  case MyCons(hd: A, tl: MyList[A])

  override def toString: String =
    @scala.annotation.tailrec
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyNil =>
          sb.result
        case MyCons(h, t) =>
          go(
            sb
              .append(h)
              .append(if t == MyNil then "]" else ", "),
            t
          )
      }
    }
    go(new StringBuilder("["), this)

object MyList:
  def apply[A](xs: A*): MyList[A] = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }

import MyList.*

def split[A](xs: MyList[A], a: A): MyList[MyList[A]] =
  @tailrec
  def reverse[A](xs: MyList[A], temp: MyList[A] = MyNil): MyList[A] =
    xs match
      case MyNil => temp
      case MyCons(hd, tl) => reverse(tl, MyCons(hd, temp))
  def go(xs: MyList[A], a: A, temp: MyList[A]): MyList[MyList[A]] =
    xs match
      case MyNil => MyCons(temp, MyNil)
      case MyCons(hd, tl) =>
        if (hd == a)
          MyCons(reverse(temp), go(tl, a, MyNil))
        else
          go(tl, a, MyCons(hd, temp))
  xs match
    case MyNil => MyNil
    case MyCons(hd, tl) => go(xs, a, MyNil)

def join[A](xs: MyList[MyList[A]], sep: A): MyList[A] =
  xs match
    case MyNil => MyNil
    case MyCons(MyNil, MyNil) => MyNil
    case MyCons(MyNil, tl) => MyCons(sep, join(tl, sep))
    case MyCons(MyCons(hd, tl), tail) => MyCons(hd, join(MyCons(tl, tail), sep))

def replaceWhere[A](xs: MyList[A], f: A => Option[A]): MyList[A] =
  xs match
    case MyNil => MyNil
    case MyCons(hd, tl) =>
      val opt = f(hd)
      opt match
        case None => MyCons(hd, replaceWhere(tl, f))
        case Some(a) => MyCons(a, replaceWhere(tl, f))

def dropWhere[A](xs: MyList[A], p: A => Boolean): MyList[A] =
  xs match
    case MyNil => MyNil
    case MyCons(hd, tl) =>
      if (p(hd))
        dropWhere(tl, p)
      else
        MyCons(hd, dropWhere(tl, p))

@main def run(): Unit =
  println(split(MyList(21,1,2,1,5,4,1), 1))