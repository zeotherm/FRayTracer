module RayTracerTests

open NUnit.Framework
open RayTracer

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

