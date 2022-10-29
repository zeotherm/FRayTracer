module RayTracerTests

open NUnit.Framework
open RayTracer

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.Pass()

[<Test>]
let TestOddSquares () =
    let expected = [1; 9; 25; 49; 81]
    let actual = squaresOfOdds [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
    Assert.That(actual, Is.EqualTo(expected))
