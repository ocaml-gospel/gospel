(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** Since we use Inferno for type inference, we must take some pre processing
    steps to deal with potential problems within Gospel specifications that are
    not caught by it. These include:

    Name resolution: In Gospel (like in OCaml), the name we use to reference a
    variable depends on where we reference it. For example, if we define a type
    [t] in a module [M], within that module, that function's name is [t], but
    outside of that module it is [M.t]. If we [open] the module, both names are
    valid. Since Inferno does not support any of these mechanisms, we must take
    care to tag every variable with a unique identifier. In our previous
    example, regardless of what name we use to reference [t], each instance of
    that variable will be tagged with the same identifier. This means when we
    build our constraints, it won't matter that the name of the type is context
    dependent, since Inferno will only look at the unique identifier. A
    consequence of this is that that there should be no unbound variables when
    we build the Inferno constraint, since to tag every variable we must keep
    track of what variables have been defined.

    Infix operators: During parsing, we use the [Tinfix] constructor to mark a
    chain of infix operators. After parsing however, this constructor is
    redundant since during typechecking, it is equivalent to two nested [Tapply]
    constructors. Therefore, to simplify building the Inferno constraint, we
    remove this constructor from all terms and replace it with [Tapply].

    Duplicate names: There are some contexts in Gospel where we are not allowed
    to introduce the same variable twice into scope (e.g. duplicate function
    arguments). Since Inferno has no way to track this natively, we must handle
    this manually.

    Type alias expansion: In the presence of some type annotation that uses some
    type [t] that is an alias for the type expression [texp], [t] is coupled
    with [texp]. This is necessary since Inferno has no way to natively type
    aliases.

    Types of top level variables: When we have specifications that use top level
    names, we insert whatever typing information is necessary in the AST. In the
    case of function applications, we insert the type of the function. In the
    case of record creation, we insert the type of the record and the type of
    each field. In the case of record field applications, we insert the type of
    the record and the type of the field. *)

val signatures :
  Namespace.env ->
  Parse_uast.s_signature ->
  Tast.s_signature * Namespace.mod_defs
(** [signature env sigs] type checks the list of top level signatures [sigs] and
    returns a map containing all the definitions within the module as well as a
    typed copy of the definitions in [sigs]. *)
