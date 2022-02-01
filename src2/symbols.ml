type content = {
  (* XXX TODO: better name than content *)
  (* unlike what is done in V1, every symbol has a type (is it a good idea?)
     and the ghost status is stored in the symbol, not the argument label *)
  name : Identifier.Ident.t;
  ty : Ttypes.ty;
  ghost : bool;
}

type symbol =
  (* the idea is to have one type for all the typed symbols
     pros:
     - pattern match to determine which is it in contrast to the boolean coding in V1 for models and constr
     - we can apply program and logical values with the same construct in the AST
     cons:
     - need to pattern match to determine which symbol it is, can't rely on type system like if there were seperate types
     - loss of organisation for the namespace
     TODO: is it all the symbols we need? *)
  | Value of content
  | Logical of content
  | Model of content
  | Constr of content

type xsymbol = { xs_ident : Identifier.Ident.t; xs_type : Ttypes.exn_type }
