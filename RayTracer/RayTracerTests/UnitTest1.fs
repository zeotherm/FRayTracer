module RayTracerTests

open NUnit.Framework
open RayTracer
open Tuples

[<SetUp>]
let Setup () =
    ()

[<Test>]
let VerifyPoint () = 
    let t = Tuple(4.3, -4.2, 3.1, 1)
    Assert.That(t.x, Is.EqualTo(4.3))
    Assert.That(t.y, Is.EqualTo(-4.2))
    Assert.That(t.z, Is.EqualTo(3.1))
    Assert.That(isPoint t, Is.True)
    Assert.That(isVector t, Is.False)

[<Test>]
let VerifyVector () = 
    let t = Tuple(4.3, -4.2, 3.1, 0)
    Assert.That(t.x, Is.EqualTo(4.3))
    Assert.That(t.y, Is.EqualTo(-4.2))
    Assert.That(t.z, Is.EqualTo(3.1))
    Assert.That(isPoint t, Is.False)
    Assert.That(isVector t, Is.True)

[<Test>]
let VerifyMakePoint () = 
    let p = makePoint 4 -4 3
    Assert.That(p, Is.EqualTo(Tuple(4, -4, 3, 1)))

[<Test>]
let VerifyMakeVector () =
    let v = makeVector 4 -4 3
    Assert.That(v, Is.EqualTo(Tuple(4, -4, 3, 0)))

[<Test>]
let AddTwoTuples () =
    let a1 = Tuple(3, -2, 5, 1)
    let a2 = Tuple(-2, 3, 1, 0)
    let expected = Tuple(1, 1, 6, 1)
    Assert.That(a1 + a2, Is.EqualTo(expected))

[<Test>]
let SubtractTwoPoints () =
    let p1 = makePoint 3 2 1
    let p2 = makePoint 5 6 7
    let p3 = p1 - p2
    let expected = makeVector -2 -4 -6
    Assert.That(isVector p3, Is.True)
    Assert.That(p3, Is.EqualTo(expected))

[<Test>]
let SubtractVectorFromPoint () =
    let p = makePoint 3 2 1
    let v = makeVector 5 6 7
    let expected = makePoint -2 -4 -6
    let r = p - v
    Assert.That(isPoint r, Is.True)
    Assert.That(r, Is.EqualTo(expected))

[<Test>]
let SubtractVectors () =
    let v1 = makeVector 3 2 1
    let v2 = makeVector 5 6 7
    let expected = makeVector -2 -4 -6
    let r = v1 - v2
    Assert.That(isVector r, Is.True)
    Assert.That(r, Is.EqualTo(expected))

[<Test>]
let SubtractFromZeroVector () = 
    let zero = makeVector 0 0 0
    let v = makeVector 1 -2 3
    let expected = makeVector -1 2 -3
    let r = zero - v
    Assert.That(isVector r, Is.True)
    Assert.That(r, Is.EqualTo(expected))

[<Test>]
let NegateTuple () =
    let t = Tuple(1, -2, 3, -4)
    Assert.That(-t, Is.EqualTo(Tuple(-1, 2, -3, 4)))

[<Test>]
let MultTupleByScalar () =
    let t = Tuple(1, -2, 3, -4)
    let expected = Tuple(3.5, -7, 10.5, -14)
    let r = t * 3.5
    Assert.That(r, Is.EqualTo(expected))

[<Test>]
let MultTupleByFraction () = 
    let t = Tuple(1, -2, 3, -4)
    let expected = Tuple(0.5, -1, 1.5, -2)
    let r = t * 0.5
    Assert.That(r, Is.EqualTo(expected))

[<Test>]
let DivTupleByScalar () = 
    let t = Tuple(1, -2, 3, -4)
    let expected = Tuple(0.5, -1, 1.5, -2)
    let r = t / 2.0
    Assert.That(r, Is.EqualTo(expected))

[<Test>]
let MagnitudeTest () =
    let uvx = makeVector 1 0 0
    let uvy = makeVector 0 1 0
    let uvz = makeVector 0 0 1
    let v1 = makeVector 1 2 3
    let v2 = makeVector -1 -2 -3
    Assert.That(magnitude uvx, Is.EqualTo(1.0))
    Assert.That(magnitude uvy, Is.EqualTo(1.0))
    Assert.That(magnitude uvz, Is.EqualTo(1.0))
    Assert.That(magnitude v1, Is.EqualTo(sqrt(14.)))
    Assert.That(magnitude v2, Is.EqualTo(sqrt(14.)))

[<Test>]
let NormTest () = 
    let v = makeVector 4 0 0
    let n = normalize v
    let expected = makeVector 1 0 0
    Assert.That(n, Is.EqualTo(expected))

[<Test>]
let NormTest2 () = 
    let v = makeVector 1 2 3
    let n = normalize v
    let invSqrt14:double = 1.0/sqrt(14.0)
    Assert.That(approx n.x invSqrt14, Is.True)
    Assert.That(approx n.y (2.0*invSqrt14), Is.True)
    Assert.That(approx n.z (3.0*invSqrt14), Is.True)

[<Test>]
let MagNormIsOne () = 
    let v = makeVector 1 2 3
    let n = normalize v
    Assert.That(magnitude n, Is.EqualTo(1.0))

[<Test>]
let DotProductTest () =
    let u = makeVector 1 2 3
    let v = makeVector 2 3 4
    Assert.That(dot u v, Is.EqualTo(20.))

[<Test>]
let CrossProductTest () =
    let u = makeVector 1 2 3
    let v = makeVector 2 3 4
    Assert.That(cross u v, Is.EqualTo(makeVector -1 2 -1))
    Assert.That(cross v u, Is.EqualTo(makeVector 1 -2 1))

