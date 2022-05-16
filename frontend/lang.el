(<variable> : <type>) ** MUST APPEAR WHEN DEFINED (eg. in function signature, let expr)
(deftype (<type_params>) <name> { <field> : <type>, ... })
(deftype (<type_params>) <name> (<variant of <type> , ...))

type = identifier | type list sep by "->" | [[ <trait name> ]]
