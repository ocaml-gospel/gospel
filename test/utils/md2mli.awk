# Extract OCaml code blocks in a markdown documentation into a .mli
# file

# If a code block is started with
#   ```ocaml invalidSyntax
# or
#   ```ocaml implementationSyntax
# it will be skipped, so that the documentation can contain invalid
# syntax (with `...` for instance) or contain .ml (implementation)
# code

# Each code block is output as a separate sub-module, so that:
# - gospel specifications in one code block are not attached to a
#   previous value by mistake
# - repeatedly declaring type `t` is not an issue

BEGIN {
  block = 0
  open = 0
}

/^ *```ocaml/,/^ *```$/ {
  if($0 ~ "^ *```ocaml.*invalidSyntax" || $0 ~ "^ *```ocaml.*implementationSyntax" ) {
    skip = 1
  } else if($0 ~ "^ *```ocaml") {
    skip = 0
    block += 1
    open = 1
    printf "module Block%d : sig\n# %d \"%s\"\n", block, FNR+1, FILENAME
  } else if($0 ~ "^ *```$" && open) {
    open = 0
    printf "end\nopen Block%d\n\n", block
  } else if(!skip) {
    print
  }
}
