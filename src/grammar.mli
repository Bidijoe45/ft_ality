type token = Word of string | Colon | Newline
type key = char
type symbol = Quit | Reset | Symbol of string
type combo_name = string
type key_mapping = (key * symbol)
type production_rule = (string list * combo_name)

val key_mapping_to_string : key_mapping -> string

module Lexer : sig
    val tokenize : in_channel -> token list
end

module Parser : sig
    val parse : token list -> (key_mapping list * production_rule list) option
end