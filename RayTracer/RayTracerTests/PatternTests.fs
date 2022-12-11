module PatternTests

open NUnit.Framework
open Pattern
open Tuples
open Color
open Ray
open Shape
open Transforms
open Matrix


[<Test>]
let SimpleStripeTest ()  =
    let s = make_stripes white black
    let (ca, cb) = match extract_patt_type s with
                   | Stripes(c_a, c_b) -> (c_a, c_b)
                   | _ -> failwith "Not a stripe pattern"
    Assert.That(ca, Is.EqualTo white)
    Assert.That(cb, Is.EqualTo black)

[<Test>]
let StripeTest () =
    let s = make_stripes white black
    // Stripes constant in y direction
    Assert.That(pattern_at s (make_point 0 0 0), Is.EqualTo white)
    Assert.That(pattern_at s (make_point 0 1 0), Is.EqualTo white)
    Assert.That(pattern_at s (make_point 0 2 0), Is.EqualTo white)
    // Stripes constant in z direction
    Assert.That(pattern_at s (make_point 0 0 0), Is.EqualTo white)
    Assert.That(pattern_at s (make_point 0 0 1), Is.EqualTo white)
    Assert.That(pattern_at s (make_point 0 0 2), Is.EqualTo white)
    // Stripes alternate in the x direction
    Assert.That(pattern_at s (make_point  0   0 0), Is.EqualTo white)
    Assert.That(pattern_at s (make_point  0.9 0 0), Is.EqualTo white)
    Assert.That(pattern_at s (make_point  1   0 0), Is.EqualTo black)
    Assert.That(pattern_at s (make_point -0.1 0 0), Is.EqualTo black)
    Assert.That(pattern_at s (make_point -1   0 0), Is.EqualTo black)
    Assert.That(pattern_at s (make_point -1.1 0 0), Is.EqualTo white)

[<Test>]
let LightingStripePattern () = 
    let pattern = make_stripes white black
    let s = make_shape Sphere
    let m = make_material [pattern] 1.0 0.0 0.0 0.0
    let eyev = make_vector 0 0 -1
    let normalv = make_vector 0 0 -1
    let light = make_pointlight (make_point 0 0 -10) (Color(1, 1, 1))
    let c1 = lighting m s light (make_point 0.9 0 0) eyev normalv false
    let c2 = lighting m s light (make_point 1.1 0 0) eyev normalv false
    Assert.That(c1, Is.EqualTo white)
    Assert.That(c2, Is.EqualTo black)

[<Test>]
let StripesWithObjXformTest () = 
    let object = make_shape Sphere |> set_shape_transform (scaling 2 2 2)
    let pattern = make_pattern Pattern.Default
    let c = pattern_at_object [pattern] object (make_point 2 3 4)
    Assert.That(c, Is.EqualTo (Color(1, 1.5, 2)))

[<Test>]
let StripesWithPatternXformTest () =
    let object = make_shape Sphere
    let pattern = make_pattern Pattern.Default |> set_patt_transform (scaling 2 2 2)
    let c = pattern_at_object [pattern] object (make_point 2 3 4)
    Assert.That(c, Is.EqualTo (Color(1, 1.5, 2)))

[<Test>]
let StripesAndSpheresWithXFormsTest () =
    let object = make_shape Sphere |> set_shape_transform (scaling 2 2 2)
    let pattern = make_pattern Pattern.Default |> set_patt_transform (translation 0.5 1 1.5)
    let c = pattern_at_object [pattern] object (make_point 2.5 3 3.5)
    Assert.That(c, Is.EqualTo (Color(0.75, 0.5, 0.25)))

[<Test>]
let DefaultPatternXformTest () =
    let pattern = make_pattern Pattern.Default
    let ident = make_ident_mat 4
    Assert.That(extract_patt_transform pattern, Is.EqualTo ident)

[<Test>]
let AssignPatternXformTest () =
    let xform = translation 1 2 3
    let pattern = make_pattern Pattern.Default |> set_patt_transform xform
    Assert.That(extract_patt_transform pattern, Is.EqualTo xform)

[<Test>]
let GradientPatternTest () =
    let pattern = make_pattern (Gradient(white, black))
    Assert.That(pattern_at pattern (make_point 0 0 0), Is.EqualTo white)
    Assert.That(pattern_at pattern (make_point 0.25 0 0), Is.EqualTo (Color(0.75, 0.75, 0.75)))
    Assert.That(pattern_at pattern (make_point 0.5 0 0), Is.EqualTo (Color(0.5, 0.5, 0.5)))
    Assert.That(pattern_at pattern (make_point 0.75 0 0), Is.EqualTo (Color(0.25, 0.25, 0.25)))

[<Test>]
let RingPatternTest () =
    let pattern = make_pattern (Rings(white, black))
    Assert.That(pattern_at pattern (make_point 0 0 0), Is.EqualTo white)
    Assert.That(pattern_at pattern (make_point 1 0 0), Is.EqualTo black)
    Assert.That(pattern_at pattern (make_point 0 0 1), Is.EqualTo black)
    Assert.That(pattern_at pattern (make_point 0.708 0 0.708), Is.EqualTo black)

[<Test>]
let CheckersPatternTest () =
    let pattern = make_pattern (Checkers(white, black))
    // Should repeat in X direction
    Assert.That(pattern_at pattern (make_point 0 0 0), Is.EqualTo white)
    Assert.That(pattern_at pattern (make_point 0.99 0 0), Is.EqualTo white)
    Assert.That(pattern_at pattern (make_point 1.01 0 0), Is.EqualTo black)
    // Should repeat in Y direction
    Assert.That(pattern_at pattern (make_point 0 0 0), Is.EqualTo white)
    Assert.That(pattern_at pattern (make_point 0 0.99 0), Is.EqualTo white)
    Assert.That(pattern_at pattern (make_point 0 1.01 0), Is.EqualTo black)
    // Should repeat in Z direction
    Assert.That(pattern_at pattern (make_point 0 0 0), Is.EqualTo white)
    Assert.That(pattern_at pattern (make_point 0 0 0.99), Is.EqualTo white)
    Assert.That(pattern_at pattern (make_point 0 0 1.01), Is.EqualTo black)