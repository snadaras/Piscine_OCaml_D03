(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   uncipher.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: snadaras <snadaras@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/03/29 23:45:49 by snadaras          #+#    #+#             *)
(*   Updated: 2018/03/29 23:45:54 by snadaras         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let is_upper c = (c >= 'A' && c <= 'Z')
let is_lower c = (c >= 'a' && c <= 'z')

let uncaesar n str =
    let rot c =
        if is_lower c
        then
            char_of_int(((int_of_char(c) - int_of_char('z') - n) mod 26) + int_of_char('z'))
        else if is_upper c
        then
            char_of_int(((int_of_char(c) - int_of_char('Z') - n) mod 26) + int_of_char('Z'))
        else
            c;
    in
    String.map rot str

let unrot42 str =
    uncaesar 42 str

let rec ft_uncrypt str f = match f with
    | [] -> str
    | h::t -> ft_uncrypt(h str) t
