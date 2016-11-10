type type_top = troll
type label_top = int * int (*number of begin and end of the name of an ast_top*)
type ast_top = 
  | Module of string * label_top * ast_top list
  | Class of string * label_top * 
  | Function of 
  | Var    
  | Const
  | Type
  | Signature
      
