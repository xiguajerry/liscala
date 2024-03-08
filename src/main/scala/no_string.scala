package me.moeyinlo

import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map}

type Symbol = String
type Number = Int | Double
type Atom = Symbol | Number
type Expr = Atom | List[_]

// TODO: Improve typing

def parse(prog: String): Expr = read_from_tokens(ListBuffer.from(tokenize(prog)))

def tokenize(s: String): List[String] = s.replace("(", " ( ").replace(")", " ) ").split(' ').filter(_.trim != "").toList

def read_from_tokens(tokens: ListBuffer[String]): Expr =
    if (tokens.isEmpty) throw RuntimeException("Unexpected EOF")
    val token = tokens.remove(0)
    token match
        case "(" =>
            val list = ListBuffer.empty[Expr]
            while tokens.head != ")" do
                list.addOne(read_from_tokens(tokens))
            tokens.remove(0)
            list.toList
        case ")" => throw RuntimeException("Unexpected )")
        case _ => atom(token)

def atom(s: String): Atom =
    try s.toInt catch
        case _ => try s.toFloat catch
            case _ => s

// TODO: Provide more functions
def standard_env =
    val env = Env(mutable.Map(), mutable.Map())
    env.inner.update("pi", Math.PI)
    env.inner.update("e", Math.E)
    env.inner.update("sin", (a: Number) =>
        a match
            case i: Int => Math.sin(i.toDouble)
            case _ => Math.sin(a.asInstanceOf[Double])
    )
    env.inner.update("cos", (a: Number) =>
        a match
            case i: Int => Math.cos(i.toDouble)
            case _ => Math.cos(a.asInstanceOf[Double])
    )
    env.inner.update("cons", (a: Any, b: List[_]) => a::b)
    env.inner.update("not", (a: Any) => if a.toString == "true" then "false" else if a.toString == "false" then "true" else throw RuntimeException())
    env.inner.update("map", (a: Any, b: List[_]) => a match
        case closure: Closure => b.map((any: Any) => closure.apply((any::Nil).asInstanceOf))
        case _ => b.map(a.asInstanceOf))
    env.inner.update("=", (a: Any, b: Any) => a == b)
    env.inner.update("+", (a: Number, b: Number) =>
        (a, b) match
            case (f: Int, s: Int) => f + s
            case (f: Double, s: Int) => f + s
            case (f: Int, s: Double) => f + s
            case (f: Double, s: Double) => f + s
    )
    env.inner.update("-", (a: Number, b: Number) =>
        (a, b) match
            case (f: Int, s: Int) => f - s
            case (f: Double, s: Int) => f - s
            case (f: Int, s: Double) => f - s
            case (f: Double, s: Double) => f - s
    )
    env.inner.update("*", (a: Number, b: Number) =>
        (a, b) match
            case (f: Int, s: Int) => f * s
            case (f: Double, s: Int) => f * s
            case (f: Int, s: Double) => f * s
            case (f: Double, s: Double) => f * s
    )
    env.inner.update("/", (a: Number, b: Number) =>
        (a, b) match
            case (f: Int, s: Int) => f / s
            case (f: Double, s: Int) => f / s
            case (f: Int, s: Double) => f / s
            case (f: Double, s: Double) => f / s
    )
    env.inner.update("<", (a: Number, b: Number) =>
        (a, b) match
            case (f: Int, s: Int) => f < s
            case (f: Double, s: Int) => f < s
            case (f: Int, s: Double) => f < s
            case (f: Double, s: Double) => f < s
    )
    env.inner.update("<=", (a: Number, b: Number) =>
        (a, b) match
            case (f: Int, s: Int) => f <= s
            case (f: Double, s: Int) => f <= s
            case (f: Int, s: Double) => f <= s
            case (f: Double, s: Double) => f <= s
    )
    env.inner.update(">", (a: Number, b: Number) =>
        (a, b) match
            case (f: Int, s: Int) => f > s
            case (f: Double, s: Int) => f > s
            case (f: Int, s: Double) => f > s
            case (f: Double, s: Double) => f > s
    )
    env.inner.update(">=", (a: Number, b: Number) =>
        (a, b) match
            case (f: Int, s: Int) => f >= s
            case (f: Double, s: Int) => f >= s
            case (f: Int, s: Double) => f >= s
            case (f: Double, s: Double) => f >= s
    )

    env

val global_env = standard_env

def lispstr(exp: Expr | Closure) = exp match
    case l: List[_] => list2str(l)
    case _ => exp.toString

def list2str(l: List[_]): String =
    var str = "("
    for e <- l do
        str += (e match
            case l1: List[_] => list2str(l1)
            case _ => e.toString)
        str += " "
    str.trim + ")"

def eval(x: Expr, env: Env = global_env): Expr | Closure = x match
    case x: String if x.head == '\"' && x.last == '\"' => throw RuntimeException("String literal is not supported yet.")
    case "true" => "true"
    case "false" => "false"
    case x: Symbol => env.find(x).asInstanceOf[Atom]
    case x if !x.isInstanceOf[List[_]] => x
    case "quote"::exp::Nil => exp.asInstanceOf[Expr]
    case "if"::test::conseq::alt::Nil =>
        val exp = if (eval(test.asInstanceOf[Expr], env).toString == "true") conseq else alt
        eval(exp.asInstanceOf[Expr], env)
    case "lambda"::body::Nil =>
        Closure(List.empty, body.asInstanceOf, env)
    case "lambda"::params::body::Nil =>
        Closure(params.asInstanceOf, body.asInstanceOf, env)
    case "define"::var1::exp::Nil =>
        env.inner.update(var1.asInstanceOf[Symbol], eval(exp.asInstanceOf[Expr], env))
        Nil
    case _ =>
        val proc = eval(x.asInstanceOf[List[Expr]].head, env)
        val args = for exp <- x.asInstanceOf[List[Expr]].drop(1) yield eval(exp, env)
        proc match
            case closure: Closure =>
                closure.apply(args.asInstanceOf)
            case _ => args match
                // shit bcz scala cannot invoke function like that in python
                case p1::Nil => proc.asInstanceOf[Any => Expr](p1)
                case p1::p2::Nil => proc.asInstanceOf[(Any, Any) => Expr](p1, p2)
                case p1::p2::p3::Nil => proc.asInstanceOf[(Any, Any, Any) => Expr](p1, p2, p3)
                case p1::p2::p3::p4::Nil => proc.asInstanceOf[(Any, Any, Any, Any) => Expr](p1, p2, p3, p4)
                case p1::p2::p3::p4::p5::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5)
                case p1::p2::p3::p4::p5::p6::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6)
                case p1::p2::p3::p4::p5::p6::p7::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7)
                case p1::p2::p3::p4::p5::p6::p7::p8::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::p41::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::p41::p42::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::p41::p42::p43::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::p41::p42::p43::p44::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::p41::p42::p43::p44::p45::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::p41::p42::p43::p44::p45::p46::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::p41::p42::p43::p44::p45::p46::p47::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46, p47)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::p41::p42::p43::p44::p45::p46::p47::p48::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46, p47, p48)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::p41::p42::p43::p44::p45::p46::p47::p48::p49::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46, p47, p48, p49)
                case p1::p2::p3::p4::p5::p6::p7::p8::p9::p10::p11::p12::p13::p14::p15::p16::p17::p18::p19::p20::p21::p22::p23::p24::p25::p26::p27::p28::p29::p30::p31::p32::p33::p34::p35::p36::p37::p38::p39::p40::p41::p42::p43::p44::p45::p46::p47::p48::p49::p50::Nil => proc.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Expr](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, p41, p42, p43, p44, p45, p46, p47, p48, p49, p50)
                case _ => throw RuntimeException(s"Unsupported invoke: $x")

// read-eval-print-loop
def repl(prompt: String = "liscala> ") =
    val scan = Scanner(System.in)
    while true do
        try
            print(prompt)
            val expr = eval(parse(s"${scan.nextLine()}"))
            if (expr != Nil) println(lispstr(expr))
        catch
            case t: Throwable => t.printStackTrace()

def gen_stuff() =
    for i <- Range(1, 51) do
        var str = "case "
        for j <- Range(1, i + 1) do
            str += s"p$j::"
        str += "Nil => proc.asInstanceOf[("
        for j <- Range(1, i + 1) do
            str += "Any, "
        str = str.dropRight(2)
        str += ") => Elem]("
        for j <- Range(1, i + 1) do
            str += s"p$j, "
        str = str.dropRight(2)
        str += ")"
        println(str)

case class Closure(params: List[Symbol], body: Expr, env: Env) {
    def apply(args: List[Atom]) =
        val newEnv = Env(mutable.Map.from(params.zip(args)), env.inner)
        eval(body, newEnv)

    override def toString: String = lispstr(body)
}

case class Env(inner: mutable.Map[Symbol, Any], outer: mutable.Map[Symbol, Any]) {
    def find(x: Symbol) = try inner(x) catch
        case _ => outer(x)
}