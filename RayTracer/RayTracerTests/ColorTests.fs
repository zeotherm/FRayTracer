module ColorTests
open NUnit.Framework
open Color
open RayTracer


[<Test>]
let VerifyColor () = 
    let c = Color(-0.5, 0.4, 1.7)
    Assert.That(c.red, Is.EqualTo(-0.5))
    Assert.That(c.green, Is.EqualTo(0.4))
    Assert.That(c.blue, Is.EqualTo(1.7))

[<Test>]
let AddColors () =
    let c1 = Color(0.9, 0.6, 0.75)
    let c2 = Color(0.7, 0.1, 0.25)
    let expected = Color(1.6, 0.7, 1.0)
    let r = c1 + c2
    Assert.That(r, Is.EqualTo(expected))

[<Test>]
let SubtractColors () =
    let c1 = Color(0.9, 0.6, 0.75)
    let c2 = Color(0.7, 0.1, 0.25)
    let expected = Color(0.2, 0.5, 0.5)
    let r = c1 - c2
    Assert.That(approx r.red expected.red, Is.True)
    Assert.That(approx r.green expected.green, Is.True)
    Assert.That(approx r.blue expected.blue, Is.True)

[<Test>]
let ScaleColors () =
    let c = Color(0.2, 0.3, 0.4)
    let expected = Color(0.4, 0.6, 0.8)
    let r = c * 2.0
    Assert.That(r, Is.EqualTo(expected))

[<Test>]
let MultiplyColors () =
    let c1 = Color(1, 0.2, 0.4)
    let c2 = Color(0.9, 1, 0.1)
    let expected = Color(0.9, 0.2, 0.04)
    let r = c1 * c2
    Assert.That(approx r.red expected.red, Is.True)
    Assert.That(approx r.green expected.green, Is.True)
    Assert.That(approx r.blue expected.blue, Is.True)
