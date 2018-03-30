(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gardening.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/03/29 23:44:50 by snadaras          #+#    #+#             *)
(*   Updated: 2018/03/29 23:44:53 by snadaras         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size tree = match tree with
    | Nil -> 0
    | Node (n, n1, n2) -> 1 + size n1 + size n2

let rec height tree = match tree with
    | Nil -> 0
    | Node (n, n1, n2) -> 1 + max (height n1) (height n2)

let draw_square x y size =
    let mid = size / 2 in
    Graphics.moveto (x + mid) (y + mid);
    Graphics.lineto (x - mid) (y + mid);
    Graphics.lineto (x - mid) (y - mid);
    Graphics.lineto (x + mid) (y - mid);
    Graphics.lineto (x + mid) (y + mid)

let draw_tree tree =
    let print_node x y str =
        draw_square x y 50;
        Graphics.moveto x y;
        Graphics.draw_string str
    in
    let rec draw_tree_node tree x y = match tree with
        | Nil               ->  Graphics.moveto (175 + x) (250 + y);
                                print_node (150 + x) (250 + y) "Nil"
        | Node (v, n1, n2)  ->  print_node (150 + x) (250 + y) v;
                                Graphics.moveto (175 + x) (250 + y);
                                Graphics.lineto (225 + x) (350 + y);
                                draw_tree_node n1 (x + 100) (y + 100 );
                                Graphics.moveto (175 + x) (250 + y);
                                Graphics.lineto (225 + x) (150 + y);
                                draw_tree_node n2 (x + 100) (y - 100)
    in
    draw_tree_node tree (-50) (-50)

let main () =  
     let a = Node ("1", Nil, Nil) in
     let b = Node ("2", a, Nil)   in
     let c = Node ("3", a, b)     in
     Printf.printf "a: Size:%d Height:%d\n" (size a) (height a);
     Printf.printf "b: Size:%d Height:%d\n" (size b) (height b);
     Printf.printf "c: Size:%d Height:%d\n" (size c) (height c);
     Graphics.open_graph (" 1400 1000 ");
     draw_tree b;
     Graphics.read_key ()

let () = 
    ignore (main ())
