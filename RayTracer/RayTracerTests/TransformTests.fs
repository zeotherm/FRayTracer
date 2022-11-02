module TransformTests

open NUnit.Framework
open System
open Transforms
open Matrix
open Tuples
open RayTracer

[<Test>]
let MultiplyByTranslationMatrixTest () =
    let t = translation 5 -3 2
    let p = make_point -3 4 5
    let res = mat_tuple_mul t p
    let expected = make_point 2 1 7
    Assert.That(is_point res, Is.True)
    Assert.That(res, Is.EqualTo(expected))

[<Test>]
let MultInvTransMatrixTest () =
    let t = translation 5 -3 2
    let ti = inverse t
    let p = make_point -3 4 5
    let res = mat_tuple_mul ti p
    let expected = make_point -8 7 3
    Assert.That(is_point res, Is.True)
    Assert.That(res, Is.EqualTo(expected))

[<Test>]
let TransNotAffectVector () =
    let t = translation 5 -3 2
    let v = make_vector -3 4 5
    let res = mat_tuple_mul t v
    Assert.That(res, Is.EqualTo v)

[<Test>]
let ScaleAPointTest () =
    let s = scaling 2 3 4
    let p = make_point -4 6 8
    let res = mat_tuple_mul s p
    let expected = make_point -8 18 32
    Assert.That(is_point res, Is.True)
    Assert.That(res, Is.EqualTo expected)

[<Test>]
let ScaleAVectorTest () =
    let s = scaling 2 3 4
    let p = make_vector -4 6 8
    let res = mat_tuple_mul s p
    let expected = make_vector -8 18 32
    Assert.That(is_vector res, Is.True)
    Assert.That(res, Is.EqualTo expected)

[<Test>]
let InvScalingTest () =
    let s = scaling 2 3 4
    let si = inverse s
    let v = make_vector -4 6 8
    let res = mat_tuple_mul si v
    let expected = make_vector -2 2 2
    Assert.That(is_vector res, Is.True)
    Assert.That(res, Is.EqualTo expected)

[<Test>]
let ReflectionNegativeScalingTest () =
    let s = scaling -1 1 1
    let p = make_point 2 3 4
    let res = mat_tuple_mul s p
    let expected = make_point -2 3 4
    Assert.That(is_point res, Is.True)
    Assert.That(res, Is.EqualTo expected)

[<Test>]
let RotateAroundXTest () =
    let p = make_point 0 1 0
    let half_quarter = rotation_x (Math.PI/4.)
    let full_quarter = rotation_x (Math.PI/2.)
    let half_res = mat_tuple_mul half_quarter p
    let full_res = mat_tuple_mul full_quarter p
    let half_expected = make_point 0 (sqrt(2.)/2.) (sqrt(2.)/2.)
    let full_expected = make_point 0 0 1
    Assert.That(is_point half_res, Is.True)
    Assert.That(is_point full_res, Is.True)
    Assert.That(approx half_res.x half_expected.x, Is.True)
    Assert.That(approx half_res.y half_expected.y, Is.True)
    Assert.That(approx half_res.z half_expected.z, Is.True)
    Assert.That(approx half_res.w half_expected.w, Is.True)
    Assert.That(approx full_res.x full_expected.x, Is.True)
    Assert.That(approx full_res.y full_expected.y, Is.True)
    Assert.That(approx full_res.z full_expected.z, Is.True)
    Assert.That(approx full_res.w full_expected.w, Is.True)

[<Test>]
let InvRotationAbtXTest () =
    let p = make_point 0 1 0
    let half_quarter = rotation_x (Math.PI/4.)
    let hqi = inverse half_quarter
    let res = mat_tuple_mul hqi p
    let expected = make_point 0 (sqrt(2.)/2.) (-sqrt(2.)/2.)
    Assert.That(is_point res, Is.True)
    Assert.That(approx res.x expected.x, Is.True)
    Assert.That(approx res.y expected.y, Is.True)
    Assert.That(approx res.z expected.z, Is.True)
    Assert.That(approx res.w expected.w, Is.True)

[<Test>]
let RotateAroundYTest () =
    let p = make_point 0 0 1
    let half_quarter = rotation_y (Math.PI/4.)
    let full_quarter = rotation_y (Math.PI/2.)
    let half_res = mat_tuple_mul half_quarter p
    let full_res = mat_tuple_mul full_quarter p
    let half_expected = make_point (sqrt(2.)/2.) 0 (sqrt(2.)/2.)
    let full_expected = make_point 1 0 0
    Assert.That(is_point half_res, Is.True)
    Assert.That(is_point full_res, Is.True)
    Assert.That(approx half_res.x half_expected.x, Is.True)
    Assert.That(approx half_res.y half_expected.y, Is.True)
    Assert.That(approx half_res.z half_expected.z, Is.True)
    Assert.That(approx half_res.w half_expected.w, Is.True)
    Assert.That(approx full_res.x full_expected.x, Is.True)
    Assert.That(approx full_res.y full_expected.y, Is.True)
    Assert.That(approx full_res.z full_expected.z, Is.True)
    Assert.That(approx full_res.w full_expected.w, Is.True)

[<Test>]
let RotateAroundZTest () =
    let p = make_point 0 1 0
    let half_quarter = rotation_z (Math.PI/4.)
    let full_quarter = rotation_z (Math.PI/2.)
    let half_res = mat_tuple_mul half_quarter p
    let full_res = mat_tuple_mul full_quarter p
    let half_expected = make_point -(sqrt(2.)/2.) (sqrt(2.)/2.) 0
    let full_expected = make_point -1 0 0
    Assert.That(is_point half_res, Is.True)
    Assert.That(is_point full_res, Is.True)
    Assert.That(approx half_res.x half_expected.x, Is.True)
    Assert.That(approx half_res.y half_expected.y, Is.True)
    Assert.That(approx half_res.z half_expected.z, Is.True)
    Assert.That(approx half_res.w half_expected.w, Is.True)
    Assert.That(approx full_res.x full_expected.x, Is.True)
    Assert.That(approx full_res.y full_expected.y, Is.True)
    Assert.That(approx full_res.z full_expected.z, Is.True)
    Assert.That(approx full_res.w full_expected.w, Is.True)

[<Test>]
let ShearXYTest () =
    let t = shearing 1 0 0 0 0 0
    let p = make_point 2 3 4
    let res = mat_tuple_mul t p
    let expected = make_point 5 3 4
    Assert.That(is_point res, Is.True)
    Assert.That(res, Is.EqualTo expected)

[<Test>]
let ShearXZTest () =
    let t = shearing 0 1 0 0 0 0
    let p = make_point 2 3 4
    let res = mat_tuple_mul t p
    let expected = make_point 6 3 4
    Assert.That(is_point res, Is.True)
    Assert.That(res, Is.EqualTo expected)

[<Test>]
let ShearYXTest () =
    let t = shearing 0 0 1 0 0 0
    let p = make_point 2 3 4
    let res = mat_tuple_mul t p
    let expected = make_point 2 5 4
    Assert.That(is_point res, Is.True)
    Assert.That(res, Is.EqualTo expected)

[<Test>]
let ShearYZTest () =
    let t = shearing 0 0 0 1 0 0
    let p = make_point 2 3 4
    let res = mat_tuple_mul t p
    let expected = make_point 2 7 4
    Assert.That(is_point res, Is.True)
    Assert.That(res, Is.EqualTo expected)

[<Test>]
let ShearZXTest () =
    let t = shearing 0 0 0 0 1 0
    let p = make_point 2 3 4
    let res = mat_tuple_mul t p
    let expected = make_point 2 3 6
    Assert.That(is_point res, Is.True)
    Assert.That(res, Is.EqualTo expected)

[<Test>]
let ShearZYTest () =
    let t = shearing 0 0 0 0 0 1
    let p = make_point 2 3 4
    let res = mat_tuple_mul t p
    let expected = make_point 2 3 7
    Assert.That(is_point res, Is.True)
    Assert.That(res, Is.EqualTo expected)

[<Test>]
let TransformsInSequenceTest () =
    let p = make_point 1 0 1
    let a = rotation_x (Math.PI/2.0)
    let b = scaling 5 5 5
    let c = translation 10 5 7
    let p2 = mat_tuple_mul a p
    let p2_e = make_point 1 -1 0
    Assert.That(approx p2.x p2_e.x, Is.True)
    Assert.That(approx p2.y p2_e.y, Is.True)
    Assert.That(approx p2.z p2_e.z, Is.True)
    Assert.That(approx p2.w p2_e.w, Is.True)
    let p3 = mat_tuple_mul b p2
    let p3_e = make_point 5 -5 0
    Assert.That(approx p3.x p3_e.x, Is.True)
    Assert.That(approx p3.y p3_e.y, Is.True)
    Assert.That(approx p3.z p3_e.z, Is.True)
    Assert.That(approx p3.w p3_e.w, Is.True)
    let p4 = mat_tuple_mul c p3
    let p4_e = make_point 15 0 7
    Assert.That(approx p4.x p4_e.x, Is.True)
    Assert.That(approx p4.y p4_e.y, Is.True)
    Assert.That(approx p4.z p4_e.z, Is.True)
    Assert.That(approx p4.w p4_e.w, Is.True)

[<Test>]
let ChainedTransformsTest () =
    let p = make_point 1 0 1
    let a = rotation_x (Math.PI/2.0)
    let b = scaling 5 5 5
    let c = translation 10 5 7
    let t = mat_mat_mul c (mat_mat_mul b a)
    let expected = make_point 15 0 7
    let res = mat_tuple_mul t p
    Assert.That(approx res.x expected.x, Is.True)
    Assert.That(approx res.y expected.y, Is.True)
    Assert.That(approx res.z expected.z, Is.True)
    Assert.That(approx res.w expected.w, Is.True)

    let comb = chain [a; b; c]
    let res2 = mat_tuple_mul comb p
    Assert.That(approx res2.x expected.x, Is.True)
    Assert.That(approx res2.y expected.y, Is.True)
    Assert.That(approx res2.z expected.z, Is.True)
    Assert.That(approx res2.w expected.w, Is.True)

