type structure = structure_item list 

type structure_item = {
    pstr_desc : structure_item_desc;
    pstr_loc : Location.t;
}

type structure_item_desc = 
  |Pstr_eval of expression * attributes
  |Pstr_value of Asttypes.rec_flag * value_binding list
  |Pstr_primitive of value_description
  | ...                        
