type typ =
  | Generic_ of trait list
  | Function_ of typ list
  | Concrete_ of {
      name : string;
      type_params : typ list
  }
and trait = string
and trait_t = trait_elem list
and trait_elem =
  | Member of {
      name: string;
      typ: typ
    }
  | Method of {
      name: string;
      typ: typ list;
    }
