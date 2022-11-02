module Transforms

open Matrix
open Tuples

let translation (x:double) (y:double) (z:double) = make_matrix [[1.;0; 0; x];
                                                                [0 ;1; 0; y];
                                                                [0; 0; 1; z];
                                                                [0; 0; 0; 1]]

let scaling (x: double) (y: double) (z: double) = make_matrix [[x; 0; 0; 0];
                                                               [0; y; 0; 0];
                                                               [0; 0; z; 0];
                                                               [0; 0; 0; 1]]

let rotation_x (r: double) = make_matrix [[1.;     0;      0;   0];
                                          [0 ; cos r; -sin r;   0];
                                          [0 ; sin r;  cos r;   0];
                                          [0 ;     0;      0;   1]]

let rotation_y (r: double) = make_matrix [[ cos r;   0; sin r;   0];
                                          [    0 ;   1;     0;   0];
                                          [-sin r;   0; cos r;   0];
                                          [    0 ;   0;     0;   1]]

let rotation_z (r: double) = make_matrix [[ cos r; -sin r;  0;   0];
                                          [ sin r;  cos r;  0;   0];
                                          [    0 ;      0;  1;   0];
                                          [    0 ;      0;  0;   1]]

let shearing x_y x_z y_x y_z z_x z_y = make_matrix [[ 1.; x_y; x_z; 0];
                                                    [y_x;   1; y_z; 0];
                                                    [z_x; z_y;   1; 0];
                                                    [  0;   0;   0; 1]]

let chain (ts: double[,] list): double[,] = 
    List.foldBack (fun acc term -> mat_mat_mul term acc) ts (make_ident_mat 4)