module SphereTests

open NUnit.Framework
open System
open Ray
open Tuples
open Transforms
open Matrix
open Sphere
open RayTracer
open Intersection

[<Test>]
let DefaultSphereTransformTest () =
    let s = make_sphere
    let I = make_ident_mat 4
    Assert.That(extract_transform s, Is.EqualTo I)

[<Test>]
let ChangeSphereTransformTest () =
    let s = make_sphere
    let t = translation 2 3 4
    let s' = s |> set_sphere_transform t
    Assert.That(extract_transform s', Is.EqualTo t)

[<Test>]
let ScaledSphereIntersectionTest () =
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let s = make_sphere
    let st = s |> set_sphere_transform (scaling 2 2 2)
    let xs = intersect st r
    Assert.That(xs.Length, Is.EqualTo 2)
    Assert.That(t_val (xs.Item(0)), Is.EqualTo 3)
    Assert.That(t_val (xs.Item(1)), Is.EqualTo 7)

[<Test>]
let TranslatedSphereIntersectionTest () =
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let s = make_sphere
    let st = s |> set_sphere_transform (translation 5 0 0)
    let xs = intersect st r
    Assert.That(xs.IsEmpty, Is.True)

[<Test>]
let NormalToSphereXAxis () =
    let s = make_sphere
    let n = normal_at s (make_point 1 0 0)
    let expected = make_vector 1 0 0
    Assert.That(n, Is.EqualTo expected)

[<Test>]
let NormalToSphereYAxis () =
    let s = make_sphere
    let n = normal_at s (make_point 0 1 0)
    let expected = make_vector 0 1 0
    Assert.That(n, Is.EqualTo expected)

[<Test>]
let NormalToSphereZAxis () =
    let s = make_sphere
    let n = normal_at s (make_point 0 0 1)
    let expected = make_vector 0 0 1
    Assert.That(n, Is.EqualTo expected)

[<Test>]
let NormalToSphereNonAxialPoint () =
    let s = make_sphere
    let p = sqrt(3.)/3.
    let n = normal_at s (make_point p p p)
    let expected = make_vector p p p
    Assert.That(approx n.x expected.x, Is.True)
    Assert.That(approx n.y expected.y, Is.True)
    Assert.That(approx n.z expected.z, Is.True)
    Assert.That(approx n.w expected.w, Is.True)

[<Test>]
let NormalOnTranslatedSphere () =
    let s = make_sphere
    let s' = s |> set_sphere_transform (translation 0 1 0)
    let n = normal_at s' (make_point 0 1.70711 -0.70711)
    let expected = make_vector 0 0.70711 -0.70711
    Assert.That(approx n.x expected.x, Is.True)
    Assert.That(approx n.y expected.y, Is.True)
    Assert.That(approx n.z expected.z, Is.True)
    Assert.That(approx n.w expected.w, Is.True)

[<Test>]
let NormalOnTransformedSphere () =
    let s = make_sphere
    let s' = s |> set_sphere_transform (chain [rotation_z (Math.PI/5.0); scaling 1 0.5 1])
    let p = sqrt(2.)/2.
    let n = normal_at s' (make_point 0 p -p)
    let expected = make_vector 0 0.97014 -0.24254
    Assert.That(approx n.x expected.x, Is.True)
    Assert.That(approx n.y expected.y, Is.True)
    Assert.That(approx n.z expected.z, Is.True)
    Assert.That(approx n.w expected.w, Is.True)

