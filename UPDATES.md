# Nameless models

When an OCaml type only has one `model`, the user can choose to not name
it.  In this case this `model` is used by default in the specifications.
I have already discussed this at length so I won't go too further.


<a id="org5a48b76"></a>

# Logical Symbols in Higher Order functions

Logical symbols such as `\/` and `/\` can be used as parameters in higher
order functions.


<a id="org686dfb2"></a>

# Models as records

I mentioned this in the breaking changes files, but when we define a
model with named fields, we also create a Gospel record type with the
same fields.


<a id="org912faae"></a>

# Bug fixes

Since this was such a deep rewrite, I probably inadvertently fixed a
bunch of bugs (and, almost definitely, introduced some new ones) but
here are the ones that I am aware of (including some things I listed
in the breaking changes file):

-   Gospel types cannot be used in OCaml interfaces
-   Fixed a bug with infix operators where the term ((3 < 4) < 5) were
    not considered different from (3 < (4 < 5)).  This is problematic
    since the first term should not type check, since this should be
    equivalent to (true < 5).
-   Fresh type variables are not allowed to be introduced in `model`
    fields.
