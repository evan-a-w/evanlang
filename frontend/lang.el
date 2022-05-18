(<variable> of <type>) | <variable> ** MUST APPEAR WHEN DEFINED (eg. in function signature, let expr)
(deftype (<type_params> <name>) (where (<type_param> is [<trait>, ...]) ...)?
  { <field> of <type>, ... })

(deftype (<type_params> <name>) (where (<type_param> is [<trait>, ...]) ...)?
  ( <variant> of <type>?, ... ))

type = identifier | type list sep by "->" | [[ <trait name> ]]
