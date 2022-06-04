(typed (<type_name>) <expr>)

(deftype (<type_params> <name>) (where (<type_param> is [<trait>, ...]) ...)?
  { <field> of <type>, ... })

(deftype (<type_params> <name>) (where (<type_param> is [<trait>, ...]) ...)?
  ( <variant> of <type>?, ... ))

type = a b identifier | type list sep by "->" | [[ <trait name>, ... ]]

egs in parser_tests.ml
