type trie

val train : Grammar.production_rule list -> trie
val run : Grammar.key_mapping list -> trie -> unit
