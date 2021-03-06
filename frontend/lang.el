(typed (<type_name>) <expr>)

(deftype (<type_params> <name>) (where (<type_param> is [<trait>, ...]) ...)?
  { <field> of <type>, ... })

(deftype (<type_params> <name>) (where (<type_param> is [<trait>, ...]) ...)?
  ( <variant> of <type>?, ... ))

(fn (<params>) <expr>)

type = a b identifier | type list sep by "->" | [[ <trait name>, ... ]]
