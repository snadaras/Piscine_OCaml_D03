(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   cipher.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/03/29 23:45:32 by snadaras          #+#    #+#             *)
(*   Updated: 2018/03/29 23:45:35 by snadaras         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let is_upper c = (c >= 'A' && c <= 'Z')
let is_lower c = (c >= 'a' && c <= 'z')

let caesar n str=
    let rot c =
        if is_lower c
        then
            char_of_int(((int_of_char(c) - int_of_char('a') + n) mod 26) + int_of_char('a'))
        else if is_upper c
        then
            char_of_int(((int_of_char(c) - int_of_char('A') + n) mod 26) + int_of_char('A'))
        else
            c;
    in
    String.map rot str

let rot42 str =
    caesar 42 str

let xor key str=
    let fxor c = char_of_int ((int_of_char c) lxor key)
    in String.map fxor str

let rec ft_crypt str f = match f with
    | [] -> str
    | h::t -> ft_crypt(h str) t

let () =
    print_endline (caesar 11 "Hello world");
    print_endline (Uncipher.uncaesar 13 (caesar 14 "you have to do "));
    print_endline (xor 18 "No one is innocent");
    print_endline (xor 24 "Born to code " );
    print_endline (ft_crypt "what is the buzz at 42" [rot42; Uncipher.unrot42]);
    print_endline (Uncipher.ft_uncrypt (ft_crypt "Try it again" 
    	[(xor 10); rot42]) [Uncipher.unrot42; (xor 10)]);
 
