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
    let s = make_sphere
    let xs = intersect s r |> List.sort
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(List.map (fun x -> t_val x) xs, Is.EqualTo([4.0; 6.0]))

[<Test>]
let RaySphereTangent () =
    let r = make_ray (make_point 0 1 -5) (make_vector 0 0 1)
    let s = make_sphere
    let xs = intersect s r |> List.sort
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(List.map (fun x -> t_val x) xs, Is.EqualTo([5.0; 5.0]))

[<Test>]
let RaySphereMiss () =
    let r = make_ray (make_point 0 2 -5) (make_vector 0 0 1)
    let s = make_sphere
    let xs = intersect s r |> List.sort
    Assert.That(xs.Length, Is.EqualTo(0))

[<Test>]
let RayInSphereDoubleIntersect () =
    let r = make_ray (make_point 0 0 0) (make_vector 0 0 1)
    let s = make_sphere
    let xs = intersect s r |> List.sort
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(List.map (fun x -> t_val x) xs, Is.EqualTo([-1.0; 1.0]))

[<Test>]
let SphereBehindRayTest () =
    let r = make_ray (make_point 0 0 5) (make_vector 0 0 1)
    let s = make_sphere
    let xs = intersect s r |> List.sort
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(List.map (fun x -> t_val x) xs, Is.EqualTo([-6.0; -4.0]))

[<Test>]
let TestIntersection () =
    let s = make_sphere
    let s_idx = id s
    let i = make_intersection 3.5 s
    Assert.That(t_val i, Is.EqualTo 3.5)
    Assert.That(object i, Is.EqualTo s_idx)

[<Test>]
let TestIntersections () =
    let s = make_sphere
    let i1 = make_intersection 1 s
    let i2 = make_intersection 2 s
    let is: Intersections = [i1; i2]
    Assert.That(is.Length, Is.EqualTo 2)
    Assert.That(t_val (is.Item(0)), Is.EqualTo 1)
    Assert.That(t_val (is.Item(1)), Is.EqualTo 2)

[<Test>]
let TestIntersectTagsObject () =
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let s = make_sphere
    let xs = intersect s r
    Assert.That(xs.Length, Is.EqualTo 2)
    Assert.That(object (xs.Item(0)), Is.EqualTo(id s))
    Assert.That(object (xs.Item(1)), Is.EqualTo(id s))

[<Test>]
let HitWhenAllTPositiveTest () =
    let s = make_sphere
    let i1 = make_intersection 1.0 s
    let i2 = make_intersection 2.0 s
    let is = [i1; i2]
    let i = hit is
    Assert.That(i, Is.EqualTo(Some(i1)))

[<Test>]
let HitWithNegativeTTest () =
    let s = make_sphere
    let i1 = make_intersection -1.0 s
    let i2 = make_intersection 1.0 s
    let is = [i1; i2]
    let i = hit is
    Assert.That(i, Is.EqualTo(Some(i2)))

[<Test>]
let HitWithAllNegTTest () =
    let s = make_sphere
    let i1 = make_intersection -2.0 s
    let i2 = make_intersection -1.0 s
    let is = [i1; i2]
    let i = hit is
    Assert.That(i, Is.EqualTo None)


[<Test>]
let RandomHitsTest () =
    let s = make_sphere
    let i1 = make_intersection 5 s
    let i2 = make_intersection 7 s
    let i3 = make_intersection -3 s
    let i4 = make_intersection 2 s
    let is = [i1; i2; i3; i4]
    let i = hit is
    Assert.That(i, Is.EqualTo(Some(i4)))
