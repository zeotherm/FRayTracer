module MatrixTests
open NUnit.Framework
open Matrix
open Tuples 
open RayTracer

[<Test>]
let CreateMatrixTest () = 
    let m = make_matrix [[ 1.0; 2.0; 3.0; 4.0]; 
                         [ 5.5; 6.5; 7.5; 8.5];
                         [ 9.0;10.0;11.0;12.0];
                         [13.5;14.5;15.5;16.5]]

    Assert.That(m[0,0], Is.EqualTo(1))
    Assert.That(m[0,3], Is.EqualTo(4))
    Assert.That(m[1,0], Is.EqualTo(5.5))
    Assert.That(m[1,2], Is.EqualTo(7.5))
    Assert.That(m[2,2], Is.EqualTo(11))
    Assert.That(m[3,0], Is.EqualTo(13.5))
    Assert.That(m[3,2], Is.EqualTo(15.5))

[<Test>]
let Create2x2MatrixTest () =
    let m = make_matrix [[-3; 5]; [1; -2]]
    Assert.That(m[0,0], Is.EqualTo(-3))
    Assert.That(m[0,1], Is.EqualTo(5))
    Assert.That(m[1,0], Is.EqualTo(1))
    Assert.That(m[1,1], Is.EqualTo(-2))

[<Test>]
let Create3x3MatrixTest () =
    let m = make_matrix [[-3; 5; 0]; [1; -2; -7]; [0; 1; 1]]
    Assert.That(m[0,0], Is.EqualTo(-3))
    Assert.That(m[1,1], Is.EqualTo(-2))
    Assert.That(m[2,2], Is.EqualTo(1))

[<Test>]
let MatrixEqualityTest () =
    let a = make_matrix [[1; 2; 3; 4];
                         [5; 6; 7; 8];
                         [9; 8; 7; 6];
                         [5; 4; 3; 2]]
    let b = make_matrix [[1; 2; 3; 4];
                         [5; 6; 7; 8];
                         [9; 8; 7; 6];
                         [5; 4; 3; 2]]
    let c = make_matrix [[2; 3; 4; 5];
                         [6; 7; 8; 9];
                         [8; 7; 6; 5];
                         [4; 3; 2; 1]]
    Assert.That(a, Is.EqualTo(b))
    Assert.That((a <> c), Is.True)

[<Test>]
let MatrixMultiplyTest () =
    let a = make_matrix [[1.; 2; 3; 4];
                         [5 ; 6; 7; 8];
                         [9 ; 8; 7; 6];
                         [5 ; 4; 3; 2]];
    let b = make_matrix [[-2.; 1; 2; 3];
                         [3 ; 2 ; 1; -1];
                         [4; 3; 6; 5];
                         [1; 2; 7; 8]]
    let c = mat_mat_mul a b
    let expected = make_matrix [[20.; 22; 50; 48];
                                [44; 54; 114; 108];
                                [40; 58; 110; 102];
                                [16;26;46;42]]
    Assert.That(c, Is.EqualTo(expected))

[<Test>]
let MatrixTupleMultTest () =
    let a = make_matrix [[1.; 2; 3; 4];
                         [2; 4; 4; 2];
                         [8; 6; 4; 1];
                         [0; 0; 0; 1]]
    let t = Tuple(1, 2, 3, 1)
    let expected = Tuple(18, 24, 33, 1)
    let res = mat_tuple_mul a t
    Assert.That(res, Is.EqualTo(expected))

[<Test>]
let MulIdentityMatrix () =
    let a = make_matrix [[0.; 1; 2; 4];
                         [1 ; 2; 4; 8];
                         [2 ; 4; 8; 16];
                         [4; 8; 16; 32]]
    let I = make_ident_mat 4
    let t = Tuple(1, 2, 3, 4)

    let a_times_i = mat_mat_mul a I
    let a_times_t = mat_tuple_mul I t

    Assert.That(a_times_i, Is.EqualTo(a))
    Assert.That(a_times_t, Is.EqualTo(t))

[<Test>]
let TransposeTest () =
    let a = make_matrix [[0.; 9; 3; 0];
                         [9; 8; 0; 8];
                         [1; 8; 5; 3];
                         [0; 0; 5; 8]]
    let I = make_ident_mat 4

    let expected = make_matrix [[0.; 9; 1; 0];
                          [9; 8; 8; 0];
                          [3; 0; 5; 5;];
                          [0; 8; 3; 8]]
    Assert.That((transpose a), Is.EqualTo(expected))
    Assert.That((transpose I), Is.EqualTo(I))

[<Test>]
let Determinant2x2Test () =
    let a = make_matrix [[1.; 5];[-3; 2]]
    let res = det a
    Assert.That(res, Is.EqualTo(17))

[<Test>]
let SubMatrixTest () =
    let a = make_matrix [[1.;5;0];[-3;2;7];[0;6;-3]]
    let a_res = submatrix 0 2 a
    let a_expected = make_matrix [[-3.;2];[0;6]]
    
    Assert.That(a_res, Is.EqualTo(a_expected))

    let b = make_matrix [[-6.; 1; 1; 6];
                         [-8; 5; 8; 6];
                         [-1; 0; 8; 2];
                         [-7; 1; -1; 1]]
    let b_res = submatrix 2 1 b
    let b_expected = make_matrix [[-6.; 1; 6];[-8; 8; 6]; [-7; -1; 1]]

    Assert.That(b_res, Is.EqualTo(b_expected))

[<Test>]
let MinorTest () = 
    let a = make_matrix [[3.;5;0];[2;-1;-7];[6;-1;5]]
    let b = submatrix 1 0 a
    Assert.That((det b), Is.EqualTo(25))
    Assert.That((minor 1 0 a), Is.EqualTo(25))

[<Test>]
let CofactorTest () =
    let a = make_matrix [[3.;5;0];[2;-1;-7];[6;-1;5]]
    Assert.That((minor 0 0 a), Is.EqualTo(-12))
    Assert.That((cofactor 0 0 a), Is.EqualTo(-12))
    Assert.That((minor 1 0 a), Is.EqualTo(25))
    Assert.That((cofactor 1 0 a), Is.EqualTo(-25))

[<Test>]
let DeterminantTest () =
    let a = make_matrix [[1.;2;6];[-5;8;-4];[2;6;4]]
    Assert.That((cofactor 0 0 a), Is.EqualTo(56))
    Assert.That((cofactor 0 1 a), Is.EqualTo(12))
    Assert.That((cofactor 0 2 a), Is.EqualTo(-46))
    Assert.That((det a), Is.EqualTo(-196))

    let b = make_matrix [[-2.;-8;3;5];[-3;1;7;3];[1;2;-9;6];[-6;7;7;-9]]
    Assert.That((cofactor 0 0 b), Is.EqualTo(690))
    Assert.That((cofactor 0 1 b), Is.EqualTo(447))
    Assert.That((cofactor 0 2 b), Is.EqualTo(210))
    Assert.That((cofactor 0 3 b), Is.EqualTo(51))
    Assert.That((det b), Is.EqualTo(-4071))

[<Test>]
let InvertabilityTest () =
    let a = make_matrix [[6.; 4;4;4];[5;5;7;6];[4;-9;3;-7];[9;1;7;-6]]
    Assert.That((det a), Is.EqualTo(-2120))
    Assert.That(is_invertable a, Is.True)

    let b = make_matrix [[-4.;2;-2;-3];[9;6;2;6];[0;-5;1;-5];[0;0;0;0]]
    Assert.That((det b), Is.EqualTo(0))
    Assert.That(is_invertable b, Is.False)

[<Test>]
let InverseTest1 () =
    let a = make_matrix [[-5.; 2; 6; -8];[1;-5;1;8];[7;7;-6;-7];[1;-3;7;4]]
    let b = inverse a
    Assert.That(det a, Is.EqualTo(532))
    Assert.That((cofactor 2 3 a), Is.EqualTo(-160))
    Assert.That(b.[3,2], Is.EqualTo(-160./532.))
    Assert.That((cofactor 3 2 a), Is.EqualTo(105))
    Assert.That(b.[2,3], Is.EqualTo(105./532.))
    let b' = make_matrix [[ 0.21805;  0.45113;  0.24060; -0.04511];
                          [-0.80827; -1.45677; -0.44361;  0.52068];
                          [-0.07895; -0.22368; -0.05263;  0.19737];
                          [-0.52256; -0.81391; -0.30075;  0.30639]]
    Assert.That((approx b.[0,0] b'.[0,0]), Is.True)
    Assert.That((approx b.[0,1] b'.[0,1]), Is.True)
    Assert.That((approx b.[0,2] b'.[0,2]), Is.True)
    Assert.That((approx b.[0,3] b'.[0,3]), Is.True)
    Assert.That((approx b.[1,0] b'.[1,0]), Is.True)
    Assert.That((approx b.[1,1] b'.[1,1]), Is.True)
    Assert.That((approx b.[1,2] b'.[1,2]), Is.True)
    Assert.That((approx b.[1,3] b'.[1,3]), Is.True)
    Assert.That((approx b.[2,0] b'.[2,0]), Is.True)
    Assert.That((approx b.[2,1] b'.[2,1]), Is.True)
    Assert.That((approx b.[2,2] b'.[2,2]), Is.True)
    Assert.That((approx b.[2,3] b'.[2,3]), Is.True)
    Assert.That((approx b.[3,0] b'.[3,0]), Is.True)
    Assert.That((approx b.[3,1] b'.[3,1]), Is.True)
    Assert.That((approx b.[3,2] b'.[3,2]), Is.True)
    Assert.That((approx b.[3,3] b'.[3,3]), Is.True)

[<Test>]
let InverseTest2 () =
    let a = make_matrix [[8.; -5; 9; 2];[7;5;6;1];[-6;0;9;6];[-3;0;-9;-4]]
    let b = inverse a
    let b' = make_matrix [[-0.15385; -0.15385; -0.28205; -0.53846];
                          [-0.07692;  0.12308;  0.02564;  0.03077];
                          [ 0.35897;  0.35897;  0.43590;  0.92308];
                          [-0.69231; -0.69231; -0.76923; -1.92308]]
    Assert.That((approx b.[0,0] b'.[0,0]), Is.True)
    Assert.That((approx b.[0,1] b'.[0,1]), Is.True)
    Assert.That((approx b.[0,2] b'.[0,2]), Is.True)
    Assert.That((approx b.[0,3] b'.[0,3]), Is.True)
    Assert.That((approx b.[1,0] b'.[1,0]), Is.True)
    Assert.That((approx b.[1,1] b'.[1,1]), Is.True)
    Assert.That((approx b.[1,2] b'.[1,2]), Is.True)
    Assert.That((approx b.[1,3] b'.[1,3]), Is.True)
    Assert.That((approx b.[2,0] b'.[2,0]), Is.True)
    Assert.That((approx b.[2,1] b'.[2,1]), Is.True)
    Assert.That((approx b.[2,2] b'.[2,2]), Is.True)
    Assert.That((approx b.[2,3] b'.[2,3]), Is.True)
    Assert.That((approx b.[3,0] b'.[3,0]), Is.True)
    Assert.That((approx b.[3,1] b'.[3,1]), Is.True)
    Assert.That((approx b.[3,2] b'.[3,2]), Is.True)
    Assert.That((approx b.[3,3] b'.[3,3]), Is.True)

[<Test>]
let InverseTest3 () =
    let a = make_matrix [[9.; 3; 0; 9];[-5;-2;-6;-3];[-4;9;6;4];[-7;6;6;2]]
    let b = inverse a
    let b' = make_matrix [[-0.04074; -0.07778;  0.14444; -0.22222];
                          [-0.07778;  0.03333;  0.36667; -0.33333];
                          [-0.02901; -0.14630; -0.10926;  0.12963];
                          [ 0.17778;  0.06667; -0.26667;  0.33333]]
    Assert.That((approx b.[0,0] b'.[0,0]), Is.True)
    Assert.That((approx b.[0,1] b'.[0,1]), Is.True)
    Assert.That((approx b.[0,2] b'.[0,2]), Is.True)
    Assert.That((approx b.[0,3] b'.[0,3]), Is.True)
    Assert.That((approx b.[1,0] b'.[1,0]), Is.True)
    Assert.That((approx b.[1,1] b'.[1,1]), Is.True)
    Assert.That((approx b.[1,2] b'.[1,2]), Is.True)
    Assert.That((approx b.[1,3] b'.[1,3]), Is.True)
    Assert.That((approx b.[2,0] b'.[2,0]), Is.True)
    Assert.That((approx b.[2,1] b'.[2,1]), Is.True)
    Assert.That((approx b.[2,2] b'.[2,2]), Is.True)
    Assert.That((approx b.[2,3] b'.[2,3]), Is.True)
    Assert.That((approx b.[3,0] b'.[3,0]), Is.True)
    Assert.That((approx b.[3,1] b'.[3,1]), Is.True)
    Assert.That((approx b.[3,2] b'.[3,2]), Is.True)
    Assert.That((approx b.[3,3] b'.[3,3]), Is.True)

[<Test>]
let InverseUndoesMultTest () =
    let a = make_matrix [[3.;-9;7;3];[3;-8;2;-9];[-4;4;4;1];[-6;5;-1;1]]
    let b = make_matrix [[8.;2;2;2];[3;-1;7;0];[7;0;5;4];[6;-2;0;5]]
    let c = mat_mat_mul a b
    let a' = mat_mat_mul c (inverse b)
    Assert.That((approx a.[0,0] a'.[0,0]), Is.True)
    Assert.That((approx a.[0,1] a'.[0,1]), Is.True)
    Assert.That((approx a.[0,2] a'.[0,2]), Is.True)
    Assert.That((approx a.[0,3] a'.[0,3]), Is.True)
    Assert.That((approx a.[1,0] a'.[1,0]), Is.True)
    Assert.That((approx a.[1,1] a'.[1,1]), Is.True)
    Assert.That((approx a.[1,2] a'.[1,2]), Is.True)
    Assert.That((approx a.[1,3] a'.[1,3]), Is.True)
    Assert.That((approx a.[2,0] a'.[2,0]), Is.True)
    Assert.That((approx a.[2,1] a'.[2,1]), Is.True)
    Assert.That((approx a.[2,2] a'.[2,2]), Is.True)
    Assert.That((approx a.[2,3] a'.[2,3]), Is.True)
    Assert.That((approx a.[3,0] a'.[3,0]), Is.True)
    Assert.That((approx a.[3,1] a'.[3,1]), Is.True)
    Assert.That((approx a.[3,2] a'.[3,2]), Is.True)
    Assert.That((approx a.[3,3] a'.[3,3]), Is.True)

    