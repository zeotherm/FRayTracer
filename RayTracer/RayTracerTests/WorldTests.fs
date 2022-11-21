﻿module WorldTests

open NUnit.Framework
open World
open Ray
open Tuples
open Color
open Sphere
open Transforms
open Intersection
open RayTracer
open Matrix
open Canvas

let halfPi = System.Math.PI/2.0

[<Test>]
let EmptyWorldTest () =
    let w = make_empty_world
    Assert.That(has_light w, Is.False)
    Assert.That(List.isEmpty (world_objects w), Is.True)

[<Test>]
let DefaultWorldTest () =
    let w = make_default_world
    let pl = make_pointlight (make_point -10 10 -10) (Color(1, 1, 1))
    let s1 = make_sphere |> set_sphere_material (make_material (Color(0.8, 1.0, 0.6)) 0.1 0.7 0.2 200.0)
    let s2 = make_sphere |> set_sphere_transform (scaling 0.5 0.5 0.5)

    Assert.That(light w, Is.EqualTo pl)    
    Assert.That(w |> world_contains s1, Is.True)
    Assert.That(w |> world_contains s2, Is.True)

[<Test>]
let IntersectWorldWithRayTest () =
    let w = make_default_world
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let res = intersect_world r w
    Assert.That(res.Length, Is.EqualTo 4)
    Assert.That(t_val (res.Item(0)), Is.EqualTo 4)
    Assert.That(t_val (res.Item(1)), Is.EqualTo 4.5)
    Assert.That(t_val (res.Item(2)), Is.EqualTo 5.5)
    Assert.That(t_val (res.Item(3)), Is.EqualTo 6)

[<Test>]
let PrecomputeIntersectionState () =
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let shape = make_sphere
    let i = make_intersection 4 shape
    let comps = prepare_computations i r
    Assert.That(extract_t comps, Is.EqualTo (t_val i))
    Assert.That(extract_obj comps, Is.EqualTo (object i))
    Assert.That(extract_point comps, Is.EqualTo (make_point 0 0 -1))
    Assert.That(extract_eyev comps, Is.EqualTo (make_vector 0 0 -1))
    Assert.That(extract_normalv comps, Is.EqualTo (make_vector 0 0 -1))

[<Test>]
let HitOccursOnTheOutsideTest () =
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let shape = make_sphere
    let i = make_intersection 4 shape
    let comps = prepare_computations i r
    Assert.That(extract_inside comps, Is.False)

[<Test>]
let HitOccursOnTheInsideTest () =
    let r = make_ray (make_point 0 0 0) (make_vector 0 0 1)
    let shape = make_sphere
    let i = make_intersection 1 shape
    let comps = prepare_computations i r
    Assert.That(extract_inside comps, Is.True)
    Assert.That(extract_point comps, Is.EqualTo (make_point 0 0 1))
    Assert.That(extract_eyev comps, Is.EqualTo (make_vector 0 0 -1))
    Assert.That(extract_normalv comps, Is.EqualTo (make_vector 0 0 -1))

[<Test>]
let IntersectionShadingTest () =
    let w = make_default_world
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let s = (world_objects w).Head
    let i = make_intersection 4 s
    let comps = prepare_computations i r
    let c = shade_hit comps w
    Assert.That(approx c.red 0.38066, Is.True)
    Assert.That(approx c.green 0.47583, Is.True)
    Assert.That(approx c.blue 0.2855, Is.True)

[<Test>]
let IntersectionInsideShadingTest () =
    let w = make_default_world |> assign_light (make_pointlight (make_point 0 0.25 0) (Color(1, 1, 1)))
    let r = make_ray (make_point 0 0 0) (make_vector 0 0 1)
    let s = (world_objects w).Tail.Head
    let i = make_intersection 0.5 s
    let comps = prepare_computations i r
    let c = shade_hit comps w
    Assert.That(approx c.red 0.90498, Is.True)
    Assert.That(approx c.green 0.90498, Is.True)
    Assert.That(approx c.blue 0.90498, Is.True)

[<Test>]
let ColorMissedRayTest () =
    let w = make_default_world
    let r = make_ray (make_point 0 0 -5) (make_vector 0 1 0)
    Assert.That(color_at w r, Is.EqualTo(Color(0,0,0)))

[<Test>]
let ColorHitRayTest () =
    let w = make_default_world
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let c = color_at w r
    Assert.That(approx c.red 0.38066, Is.True)
    Assert.That(approx c.green 0.47583, Is.True)
    Assert.That(approx c.blue 0.2855, Is.True)

[<Test>]
let ColorInnerHitTest () =
    let pl = make_pointlight (make_point -10 10 -10) (Color(1, 1, 1))
    let s1 = make_sphere |> set_sphere_material (make_material (Color(0.8, 1.0, 0.6)) 1.0 0.7 0.2 200.0)
    let s2 = make_sphere |> set_sphere_material (make_material (Color(1, 1, 1))       1.0 0.9 0.9 200.0)
                         |> set_sphere_transform (scaling 0.5 0.5 0.5)

    let w = make_world [pl] [s1;s2]
    let inner_color = (world_objects w).Item(1) |> extract_material |> mat_color
    let r = make_ray (make_point 0 0 0.75) (make_vector 0 0 -1)
    let c = color_at w r
    Assert.That(c, Is.EqualTo inner_color)

[<Test>]
let CameraConstructionTest () =
    let width = 160
    let height = 120
    let fov = halfPi
    let c = make_camera width height fov
    Assert.That(extract_height c, Is.EqualTo height)
    Assert.That(extract_width c, Is.EqualTo width)
    Assert.That(extract_field_of_view c, Is.EqualTo fov)
    Assert.That(extract_cam_transform c, Is.EqualTo (make_ident_mat 4))

[<Test>]
let HorizontalPixelTest () =
    let c = make_camera 200 125 halfPi
    Assert.That(approx (pixel_size c) 0.01, Is.True)

[<Test>]
let VerticalPixelTest () =
    let c = make_camera 125 200 halfPi
    Assert.That(approx (pixel_size c) 0.01, Is.True) 
    
[<Test>]
let ConstructRayThroughCenterTest () =
    let c = make_camera 201 101 halfPi
    let r = ray_for_pixel c 100 50
    Assert.That(origin r, Is.EqualTo (make_point 0 0 0))
    let dr = direction r
    Assert.That(approx dr.x 0, Is.True)
    Assert.That(approx dr.y 0, Is.True)
    Assert.That(approx dr.z -1, Is.True)


[<Test>]
let ConstructRayThroughCornerTest () =
    let c = make_camera 201 101 halfPi
    let r = ray_for_pixel c 0 0
    Assert.That(origin r, Is.EqualTo (make_point 0 0 0))
    let dr = direction r
    Assert.That(approx dr.x 0.66519, Is.True)
    Assert.That(approx dr.y 0.33259, Is.True)
    Assert.That(approx dr.z -0.66851, Is.True)

[<Test>]
let ConstructRayWithXformedCameraTest () =
    let c = make_camera 201 101 halfPi 
    let xform = chain [translation 0 -2 5; rotation_y (System.Math.PI/4.)]
    let c' = set_camera_transform xform c

    let r = ray_for_pixel c' 100 50
    Assert.That(origin r, Is.EqualTo (make_point 0 2 -5))
    let halfRt2 = System.Math.Sqrt(2.)/2.
    let dr = direction r
    Assert.That(approx dr.x halfRt2, Is.True)
    Assert.That(approx dr.y 0., Is.True)
    Assert.That(approx dr.z -halfRt2, Is.True)

[<Test>]
let RenderDefaultWorldTest () =
    let w = make_default_world
    let from = make_point 0 0 -5
    let to2 = origin000
    let up = make_vector 0 1 0
    let c = make_camera 11 11 halfPi |> set_camera_transform (view_transform from to2 up)
    let img = render c w
    let res = pixel_at (5, 5) img 
    let expected = Color(0.38066, 0.47583, 0.2855)
    Assert.That(approx res.red expected.red, Is.True)
    Assert.That(approx res.green expected.green, Is.True)
    Assert.That(approx res.blue expected.blue, Is.True)
   
[<Test>]
let NoShadowCollinearTest () =
    let w = make_default_world
    let p = make_point 0 10 0
    Assert.That(is_shadowed w p, Is.False)

[<Test>]
let PointInShadowTest () =
    let w = make_default_world
    let p = make_point 10 -10 10
    Assert.That(is_shadowed w p, Is.True)

[<Test>]
let LightBetweenObjectPointShadowTest () =
    let w = make_default_world
    let p = make_point -20 20 -20
    Assert.That(is_shadowed w p, Is.False)

[<Test>]
let ObjectBehindPointShadowTest () =
    let w = make_default_world
    let p = make_point -2 2 -2
    Assert.That(is_shadowed w p, Is.False)

[<Test>]
let ShadeHitDealsWithShadowsTest () =
    let p = make_pointlight (make_point 0 0 -10) (Color(1,1,1))
    let s1 = make_sphere
    let s2 = make_sphere |> set_sphere_transform (translation 0 0 10)
    let w = make_world [p] [s1; s2]
    let r = make_ray (make_point 0 0 5) (make_vector 0 0 1)
    let i = make_intersection 4 s2
    let comps = prepare_computations i r
    let c = shade_hit comps w
    Assert.That(c, Is.EqualTo (Color(0.1, 0.1, 0.1)))

[<Test>]
let HitShouldOffsetTest () =
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let s = make_sphere |> set_sphere_transform (translation 0 0 1)
    let i = make_intersection 5 s
    let comps = prepare_computations i r
    let pt = extract_point comps
    let ov_pt = extract_over_point comps
    Assert.That(ov_pt.z, Is.LessThan (-EPSILON/2.0))
    Assert.That(pt.z, Is.GreaterThan ov_pt.z)


