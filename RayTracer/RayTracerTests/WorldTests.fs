module WorldTests

open NUnit.Framework
open World
open Ray
open Tuples
open Color
open Shape
open Transforms
open Intersection
open RayTracer
open Matrix
open Canvas
open Pattern

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
    let wos = world_objects w
    let s1 = wos.Item(0)
    let s2 = wos.Item(1)

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
    let shape = make_shape Sphere
    let i = make_intersection 4 shape
    let comps = prepare_computations i r [i]
    Assert.That(extract_t comps, Is.EqualTo (t_val i))
    Assert.That(extract_obj comps, Is.EqualTo (object i))
    Assert.That(extract_point comps, Is.EqualTo (make_point 0 0 -1))
    Assert.That(extract_eyev comps, Is.EqualTo (make_vector 0 0 -1))
    Assert.That(extract_normalv comps, Is.EqualTo (make_vector 0 0 -1))

[<Test>] 
let PrecomputeIntersectionState2 () =
    let shape = make_shape Plane
    let root2 = sqrt 2.0
    let half_root2 = root2/2.0
    let r = make_ray (make_point 0 1 -1) (make_vector 0 -half_root2 half_root2)
    let i = make_intersection root2 shape
    let comps = prepare_computations i r [i]
    let reflectv = extract_reflectv comps
    Assert.That(approx reflectv.x 0.0, Is.True)
    Assert.That(approx reflectv.y half_root2, Is.True)
    Assert.That(approx reflectv.z half_root2, Is.True)

[<Test>]
let HitOccursOnTheOutsideTest () =
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let shape = make_shape Sphere
    let i = make_intersection 4 shape
    let comps = prepare_computations i r [i]
    Assert.That(extract_inside comps, Is.False)

[<Test>]
let HitOccursOnTheInsideTest () =
    let r = make_ray (make_point 0 0 0) (make_vector 0 0 1)
    let shape = make_shape Sphere
    let i = make_intersection 1 shape
    let comps = prepare_computations i r [i]
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
    let comps = prepare_computations i r [i]
    let c = shade_hit comps w REC_LIMIT
    Assert.That(approx c.red 0.38066, Is.True)
    Assert.That(approx c.green 0.47583, Is.True)
    Assert.That(approx c.blue 0.2855, Is.True)

[<Test>]
let IntersectionInsideShadingTest () =
    let w = make_default_world |> assign_light (make_pointlight (make_point 0 0.25 0) (Color(1, 1, 1)))
    let r = make_ray (make_point 0 0 0) (make_vector 0 0 1)
    let s = (world_objects w).Tail.Head
    let i = make_intersection 0.5 s
    let comps = prepare_computations i r [i]
    let c = shade_hit comps w REC_LIMIT
    Assert.That(approx c.red 0.90498, Is.True)
    Assert.That(approx c.green 0.90498, Is.True)
    Assert.That(approx c.blue 0.90498, Is.True)

[<Test>]
let ColorMissedRayTest () =
    let w = make_default_world
    let r = make_ray (make_point 0 0 -5) (make_vector 0 1 0)
    Assert.That(color_at w r REC_LIMIT, Is.EqualTo(Color(0,0,0)))

[<Test>]
let ColorHitRayTest () =
    let w = make_default_world
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let c = color_at w r REC_LIMIT
    Assert.That(approx c.red 0.38066, Is.True)
    Assert.That(approx c.green 0.47583, Is.True)
    Assert.That(approx c.blue 0.2855, Is.True)

[<Test>]
let ColorInnerHitTest () =
    let pl = make_pointlight (make_point -10 10 -10) (Color(1, 1, 1))
    let s1 = make_shape Sphere |> set_shape_material (make_material [make_pattern (Solid(Color(0.8, 1.0, 0.6)))] 1.0 0.7 0.2 200.0 0.0 0.0 1.0)
    let s2 = make_shape Sphere |> set_shape_material (make_material [make_pattern (Solid(Color(1, 1, 1)))] 1.0 0.9 0.9 200.0 0.0 0.0 1.0)
                               |> set_shape_transform (scaling 0.5 0.5 0.5)

    let w = make_world [pl] [s1;s2]
    let inner_color = (world_objects w).Item(1) |> extract_material |> mat_color
    let r = make_ray (make_point 0 0 0.75) (make_vector 0 0 -1)
    let c = color_at w r REC_LIMIT
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
    let s1 = make_shape Sphere
    let s2 = make_shape Sphere |> set_shape_transform (translation 0 0 10)
    let w = make_world [p] [s1; s2]
    let r = make_ray (make_point 0 0 5) (make_vector 0 0 1)
    let i = make_intersection 4 s2
    let comps = prepare_computations i r [i]
    let c = shade_hit comps w REC_LIMIT
    Assert.That(c, Is.EqualTo (Color(0.1, 0.1, 0.1)))

[<Test>]
let HitShouldOffsetTest () =
    let local_EPSILON = 0.00001
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let s = make_shape Sphere |> set_shape_transform (translation 0 0 1)
    let i = make_intersection 5 s
    let comps = prepare_computations i r [i]
    let pt = extract_point comps
    let ov_pt = extract_over_point comps
    Assert.That(ov_pt.z, Is.LessThan (-local_EPSILON/2.0))
    Assert.That(pt.z, Is.GreaterThan ov_pt.z)

[<Test>]
let NoReflectionOnNonReflectiveSurfaceTest () =
    let w = make_default_world
    let r = make_ray (make_point 0 0 0) (make_vector 0 0 1)
    let shape = (world_objects w).Item(1)
    let shape_mod = set_shape_material (override_ambient 1.0 (extract_material shape)) shape
    let i = make_intersection 1 shape_mod
    let comps = prepare_computations i r [i]
    let color = reflected_color w comps REC_LIMIT
    Assert.That(color, Is.EqualTo black)

[<Test>]
let ReflectedColorOffReflectiveSurfaceTest () =
    let root2 = sqrt 2.
    let halfRt2 = root2/2.
    let mod_mat = make_def_material |> override_reflective 0.5
    let s = make_shape Plane 
            |> set_shape_material mod_mat
            |> set_shape_transform (translation 0 -1 0)
    let w = make_default_world |> add_object s
    let r = make_ray (make_point 0 0 -3) (make_vector 0 -halfRt2 halfRt2)
    let i = make_intersection root2 s
    let comps = prepare_computations i r [i]
    let color = shade_hit comps w REC_LIMIT
    Assert.That(approx color.red 0.87677, Is.True)
    Assert.That(approx color.green 0.92436, Is.True)
    Assert.That(approx color.blue 0.82918, Is.True)

[<Test>]
let MututallyReflectiveSurfaces () =
    let light = make_pointlight (make_point 0 0 0) white
    let mirror = make_def_material |> override_reflective 1.0
    let lower = make_shape Plane 
                |> set_shape_material mirror
                |> set_shape_transform (translation 0 -1 0)
    let upper = make_shape Plane
                |> set_shape_material mirror
                |> set_shape_transform (translation 0 1 0)
    let w = make_world [light] [lower; upper]
    let r = make_ray (make_point 0 0 0) (make_vector 0 1 0)
    let c = color_at w r REC_LIMIT 
    Assert.That(c, Is.EqualTo (Color(13.3, 13.3, 13.3)))

[<Test>]
let ReflectedColorAtMaxDepth () =
    let root2 = sqrt 2.
    let halfRt2 = root2/2.
    let mod_mat = make_def_material |> override_reflective 0.5
    let s = make_shape Plane 
            |> set_shape_material mod_mat
            |> set_shape_transform (translation 0 -1 0)
    let w = make_default_world |> add_object s
    let r = make_ray (make_point 0 0 -3) (make_vector 0 -halfRt2 halfRt2)
    let i = make_intersection root2 s
    let comps = prepare_computations i r [i]
    let color = reflected_color w comps 0
    Assert.That(color, Is.EqualTo black)

[<Test>]
let RefractiveIndexOverlappingSpheresTest () =
    let A = make_glass_sphere 
            |> set_shape_transform (scaling 2 2 2) 
            |> set_shape_material (make_glass_material |> override_refractive_idx 1.5)
    let B = make_glass_sphere
            |> set_shape_transform (translation 0 0 -0.25)
            |> set_shape_material (make_glass_material |> override_refractive_idx 2.0)
    let C = make_glass_sphere
            |> set_shape_transform (translation 0 0 0.25)
            |> set_shape_material (make_glass_material |> override_refractive_idx 2.5)
    let r = make_ray (make_point 0 0 -4) (make_vector 0 0 1)
    let xs = [make_intersection 2 A;
              make_intersection 2.75 B;
              make_intersection 3.25 C;
              make_intersection 4.75 B;
              make_intersection 5.25 C;
              make_intersection 6 A]
    let comp0 = prepare_computations (xs.Item(0)) r xs
    Assert.That(extract_n1 comp0, Is.EqualTo 1.0)
    Assert.That(extract_n2 comp0, Is.EqualTo 1.5)
    let comp1 = prepare_computations (xs.Item(1)) r xs
    Assert.That(extract_n1 comp1, Is.EqualTo 1.5)
    Assert.That(extract_n2 comp1, Is.EqualTo 2.0)
    let comp2 = prepare_computations (xs.Item(2)) r xs
    Assert.That(extract_n1 comp2, Is.EqualTo 2.0)
    Assert.That(extract_n2 comp2, Is.EqualTo 2.5)
    let comp3 = prepare_computations (xs.Item(3)) r xs
    Assert.That(extract_n1 comp3, Is.EqualTo 2.5)
    Assert.That(extract_n2 comp3, Is.EqualTo 2.5)
    let comp4 = prepare_computations (xs.Item(4)) r xs
    Assert.That(extract_n1 comp4, Is.EqualTo 2.5)
    Assert.That(extract_n2 comp4, Is.EqualTo 1.5)
    let comp5 = prepare_computations (xs.Item(5)) r xs
    Assert.That(extract_n1 comp5, Is.EqualTo 1.5)
    Assert.That(extract_n2 comp5, Is.EqualTo 1.0)

[<Test>]
let UnderPointisOffsetBeneathSurfaceTest () =
    let local_EPSILON = 0.00001
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let shape = make_glass_sphere |> set_shape_transform (translation 0 0 1)
    let i = make_intersection 5.0 shape
    let xs = [i]
    let comps = prepare_computations i r xs
    let under_pt = extract_under_point comps
    let pt = extract_point comps

    Assert.That(under_pt.z, Is.GreaterThan(local_EPSILON/2.0))
    Assert.That(pt.z, Is.LessThan(under_pt.z))

[<Test>]
let RefractedColorOfOpaqueObjectTest () =
    let w = make_default_world
    let shape = w|> world_objects |> List.head
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let xs = [make_intersection 4.0 shape; make_intersection 6.0 shape]
    let comps = prepare_computations (xs.Item(0)) r xs
    let c = refracted_color w comps 5
    Assert.That(c, Is.EqualTo black)

[<Test>]
let RefractedColorAtMaxDepthTest () =
    let w = make_default_world
    let shape = w |> world_objects |> List.head
                |> set_shape_material make_glass_material
    let r = make_ray (make_point 0 0 -5) (make_vector 0 0 1)
    let xs = [make_intersection 4.0 shape; make_intersection 6.0 shape]
    let comps = prepare_computations (xs.Item(0)) r xs
    let c = refracted_color w comps 0
    Assert.That(c, Is.EqualTo black)

[<Test>]
let RefractedColorUnderTotalInternalReflectionTest () =
    let root2 = sqrt 2.
    let halfRt2 = root2/2.
    let w = make_default_world
    let shape = w |> world_objects |> List.head
                |> set_shape_material make_glass_material
    let r = make_ray (make_point 0 0 halfRt2) (make_vector 0 1 0)
    let xs = [make_intersection -halfRt2 shape; make_intersection halfRt2 shape]
    let comps = prepare_computations (xs.Item(1)) r xs
    let c = refracted_color w comps 5
    Assert.That(c, Is.EqualTo black)

[<Test>]
let RefractedColorFromRefractedRay () =
    let wd = make_default_world
    let A_raw = wd |> world_objects |> List.head 
    let A_mat = extract_material A_raw |> override_ambient 1.0 |> override_pattern [make_pattern Default]
    let A = A_raw |> set_shape_material A_mat
    let B = (world_objects wd).Item(1) |> set_shape_material make_glass_material
    let pl = lights wd
    let w = make_world pl [A; B]
    let r = make_ray (make_point 0 0 0.1) (make_vector 0 1 0)
    let xs = [make_intersection -0.9899 A; make_intersection -0.4899 B; make_intersection 0.4899 B; make_intersection 0.9899 A]
    let comps = prepare_computations (xs.Item(2)) r xs
    let c = refracted_color w comps 5
    Assert.That(approx c.red 0, Is.True)
    Assert.That(approx c.green 0.99888, Is.True)
    Assert.That(approx c.blue 0.04725, Is.True)

[<Test>]
let ShadeHitWithTranparentMaterialTest () =
    let root2 = sqrt 2.
    let halfRt2 = root2/2.
    let wd = make_default_world
    let floor_mat = make_glass_material |> override_transparancy 0.5
    let floor = make_shape Plane |> set_shape_transform (translation 0 -1 0) |> set_shape_material floor_mat
    let mat = make_def_material |> override_ambient 0.5 |> override_color (Color(1,0,0))
    let ball = make_shape Sphere |> set_shape_transform (translation 0 -3.5 -0.5) |> set_shape_material mat
    let w = wd |> add_object floor |> add_object ball
    let r = make_ray (make_point 0 0 -3) (make_vector 0 -halfRt2 halfRt2)
    let xs = make_intersection root2 floor
    let comps = prepare_computations xs r [xs]
    let c = shade_hit comps w 5
    Assert.That(approx c.red 0.93642, Is.True)
    Assert.That(approx c.green 0.68642, Is.True)
    Assert.That(approx c.blue 0.68642, Is.True)

