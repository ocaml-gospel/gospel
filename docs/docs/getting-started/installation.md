---
sidebar_position: 1
---

# Installing Gospel

Please make sure that you already have a decently recent version of OCaml and
Opam installed. Gospel requires the following versions:

- OCaml 4.11 or newer
- Opam 2.0 or newer

Please visit [ocaml.org](https://ocaml.org/learn/tutorials/up_and_running.html)
for instructions on how to get started with your OCaml environment.

Gospel is available on Opam repositories. Installing it is straightforward:

```shell
$ opam install gospel
```

This will install the `gospel` tool binary, as well as the developer library
if you wish to build your software on top of Gospel. You may check the
installation with:

```shell
$ gospel --version
gospel version 0.1.0
```
