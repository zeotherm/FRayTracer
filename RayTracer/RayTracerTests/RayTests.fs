module RayTests
open NUnit.Framework
open Ray
open Tuples
open Transforms
open Color
open Sphere
open RayTracer
open Intersection

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
    let xs = intersect s r |> List.sortBy (fun i -> t_val i)
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(List.map (fun x -> t_val x) xs, Is.EqualTo([4.0; 6.0]))

[<Test>]
let RaySphereTangent () =
    let r = make_ray (make_point 0 1 -5) (make_vector 0 0 1)
    let s = make_sphere
    let xs = intersect s r |> List.sortBy (fun i -> t_val i)
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(List.map (fun x -> t_val x) xs, Is.EqualTo([5.0; 5.0]))

[<Test>]
let RaySphereMiss () =
    let r = make_ray (make_point 0 2 -5) (make_vector 0 0 1)
    let s = make_sphere
    let xs = intersect s r |> List.sortBy (fun i -> t_val i)
    Assert.That(xs.Length, Is.EqualTo(0))

[<Test>]
let RayInSphereDoubleIntersect () =
    let r = make_ray (make_point 0 0 0) (make_vector 0 0 1)
    let s = make_sphere
    let xs = intersect s r |> List.sortBy (fun i -> t_val i)
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(List.map (fun x -> t_val x) xs, Is.EqualTo([-1.0; 1.0]))

[<Test>]
let SphereBehindRayTest () =
    let r = make_ray (make_point 0 0 5) (make_vector 0 0 1)
    let s = make_sphere
    let xs = intersect s r |> List.sortBy (fun i -> t_val i)
    Assert.That(xs.Length, Is.EqualTo(2))
    Assert.That(List.map (fun x -> t_val x) xs, Is.EqualTo([-6.0; -4.0]))

[<Test>]
let TestIntersection () =
    let s = make_sphere
    let s_idx = id s
    let i = make_intersection 3.5 s
    Assert.That(t_val i, Is.EqualTo 3.5)
    Assert.That(object i, Is.EqualTo s)

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
    Assert.That(object (xs.Item(0)), Is.EqualTo s)
    Assert.That(object (xs.Item(1)), Is.EqualTo s)

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

[<Test>]
let RayTransTest () = 
    let r = make_ray (make_point 1 2 3) (make_vector 0 1 0)
    let m = translation 3 4 5
    let r' = transform r m
    let expected = make_ray (make_point 4 6 8) (make_vector 0 1 0)
    Assert.That(origin r', Is.EqualTo (origin expected))
    Assert.That(direction r', Is.EqualTo (direction expected))

[<Test>]
let RayScaleTest () =
    let r = make_ray (make_point 1 2 3) (make_vector 0 1 0)
    let m = scaling 2 3 4
    let r' = transform r m
    let expected = make_ray (make_point 2 6 12) (make_vector 0 3 0)
    Assert.That(origin r', Is.EqualTo (origin expected))
    Assert.That(direction r', Is.EqualTo (direction expected))

[<Test>]
let ReflectAt45Deg () =
    let v = make_vector 1 -1 0
    let n = make_vector 0 1 0
    let r = reflect v n
    let expected = make_vector 1 1 0
    Assert.That(r, Is.EqualTo expected)

[<Test>]
let ReflectOffSlant () =
    let v = make_vector 0 -1 0
    let half_rt2 = sqrt(2.)/2.
    let n = make_vector half_rt2 half_rt2 0
    let r = reflect v n
    let expected = make_vector 1 0 0
    Assert.That(approx r.x expected.x, Is.True)
    Assert.That(approx r.y expected.y, Is.True)
    Assert.That(approx r.z expected.z, Is.True)
    Assert.That(approx r.w expected.w, Is.True)

[<Test>]
let PointLightTest () =
    let i = Color(1, 1, 1)
    let p = make_point 0 0 0
    let pl = make_pointlight p i
    Assert.That(intensity pl, Is.EqualTo i)
    Assert.That(location pl, Is.EqualTo p)

[<Test>]
let DefaultMaterialTest () =
    let m = make_def_material
    Assert.That(mat_color m, Is.EqualTo (Color(1, 1, 1)))
    Assert.That(ambient m, Is.EqualTo 0.1)
    Assert.That(diffuse m, Is.EqualTo 0.9)
    Assert.That(specular m, Is.EqualTo 0.9)
    Assert.That(shininess m, Is.EqualTo 200.0)

[<Test>]
let SphereDefaultMaterial () =
    let s = make_sphere
    let m = extract_material s
    Assert.That(m, Is.EqualTo make_def_material)

[<Test>]
let SphereAssignedMaterial () =
    let s = make_sphere
    let m = make_def_material
    let m' = override_ambient m 1.0
    let s' = s|> set_sphere_material m'
    Assert.That(extract_material s', Is.EqualTo m')

[<Test>]
let LightTest1 () =
    let m = make_def_material
    let position = make_point 0 0 0
    let eyev = make_vector 0 0 -1
    let normalv = make_vector 0 0 -1
    let light = make_pointlight (make_point 0 0 -10) (Color(1, 1, 1))
    let result = lighting m light position eyev normalv false
    let expected = Color(1.9, 1.9, 1.9)
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let LightTest2 () =
    let m = make_def_material
    let position = make_point 0 0 0
    let half_rt2 = sqrt(2.)/2.
    let eyev = make_vector 0 half_rt2 -half_rt2
    let normalv = make_vector 0 0 -1
    let light = make_pointlight (make_point 0 0 -10) (Color(1, 1, 1))
    let result = lighting m light position eyev normalv false
    let expected = Color(1.0, 1.0, 1.0)
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let LightTest3 () =
    let m = make_def_material
    let position = make_point 0 0 0
    let eyev = make_vector 0 0 -1
    let normalv = make_vector 0 0 -1
    let light = make_pointlight (make_point 0 10 -10) (Color(1, 1, 1))
    let result = lighting m light position eyev normalv false
    let expected = Color(0.7364, 0.7364, 0.7364)
    Assert.That(approx result.red expected.red, Is.True)
    Assert.That(approx result.blue expected.blue, Is.True)
    Assert.That(approx result.green expected.green, Is.True)

[<Test>]
let LightTest4 () =
    let m = make_def_material
    let position = make_point 0 0 0
    let half_rt2 = sqrt(2.)/2.
    let eyev = make_vector 0 -half_rt2 -half_rt2
    let normalv = make_vector 0 0 -1
    let light = make_pointlight (make_point 0 10 -10) (Color(1, 1, 1))
    let result = lighting m light position eyev normalv false
    let expected = Color(1.6364, 1.6364, 1.6364)
    Assert.That(approx result.red expected.red, Is.True)
    Assert.That(approx result.blue expected.blue, Is.True)
    Assert.That(approx result.green expected.green, Is.True)

[<Test>]
let LightTest5 () =
    let m = make_def_material
    let position = make_point 0 0 0
    let eyev = make_vector 0 0 -1
    let normalv = make_vector 0 0 -1
    let light = make_pointlight (make_point 0 0 10) (Color(1, 1, 1))
    let result = lighting m light position eyev normalv false
    let expected = Color(0.1, 0.1, 0.1)
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ShadowTest1 () =
    let m = make_def_material
    let position = make_point 0 0 0
    let eyev = make_vector 0 0 -1
    let normalv = make_vector 0 0 -1
    let light = make_pointlight (make_point 0 0 -10) (Color(1, 1, 1))
    let in_shadow = true
    let result = lighting m light position eyev normalv in_shadow
    let expected = Color(0.1, 0.1, 0.1)
    Assert.That(result, Is.EqualTo expected)



