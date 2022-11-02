module RayTests
open NUnit.Framework
open Ray
open Tuples

[<Test>]
let RayTypeTest () =
    let o = make_point 1 2 3
    let d = make_vector 4 5 6
    let r = make_ray o d
    Assert.That(origin r, Is.EqualTo o)
    Assert.That(direction r, Is.EqualTo d)

[<Test>]
let RayPositionTest () =
    let r = make_ray (make_point 2 3 4) (make_vector 1 0 0)
    Assert.That((position r 0), Is.EqualTo(make_point 2 3 4))
    Assert.That((position r 1), Is.EqualTo(make_point 3 3 4))
    Assert.That((position r -1), Is.EqualTo(make_point 1 3 4))
    Assert.That((position r 2.5), Is.EqualTo(make_point 4.5 3 4))

[<Test>]
let RaySphereDoubleIntersect () =
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let s = sphere()
    let xs = intersect s r |> List.sort
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(xs, Is.EqualTo([4.0; 6.0]))

[<Test>]
let RaySphereTangent () =
    let r = make_ray (make_point 0 1 -5) (make_vector 0 0 1)
    let s = sphere()
    let xs = intersect s r |> List.sort
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(xs, Is.EqualTo([5.0; 5.0]))

[<Test>]
let RaySphereMiss () =
    let r = make_ray (make_point 0 2 -5) (make_vector 0 0 1)
    let s = sphere()
    let xs = intersect s r |> List.sort
    Assert.That(xs.Length, Is.EqualTo(0))

[<Test>]
let RayInSphereDoubleIntersect () =
    let r = make_ray (make_point 0 0 0) (make_vector 0 0 1)
    let s = sphere()
    let xs = intersect s r |> List.sort
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(xs, Is.EqualTo([-1.0; 1.0]))

[<Test>]
let SphereBehindRayTest () =
    let r = make_ray (make_point 0 0 5) (make_vector 0 0 1)
    let s = sphere()
    let xs = intersect s r |> List.sort
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(xs, Is.EqualTo([-6.0; -4.0]))


    