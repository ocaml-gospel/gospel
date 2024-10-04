Presents: Tiago, Nicolas
- we've decided to merge https://github.com/ocaml-gospel/gospel/pull/391
- other PRs (#418, #419 and #420) won't be reviewed due to lack of time.
- next release should contain:
  + what has already been merged since last release
  + the newly merged #391
  + improvements of the typing of records (#418, #419 and #420) and another one yet to be written (Nicolas has most of the code)
  + maybe remove the coercion mechanism for the user?
  + the new standard library (PR not yet ready) axiomatized with Coq proof
  + should Gospel repo contains the Coq proof?
- we don't have a timeline for the release -- we'll ship it when it is ready
- the release contains breaking changes for Ortac, it's ok:
  + The current released version of Ortac asks for exactly Gospel.0.3.0, so a new Gospel version won't break it.
  + Nicolas will update and adapt Ortac accordingly
- discussion on documentation setup:
  + Tiago wants to add some examples and an experimental section about what should be in future versions of Gospel.
  + The documentation is using docusaurus
  + Simply modify or add a markdown file in `doc/docs/`.
  + docusaurus is using the file hierarchy to build the website structure
  + position in the sidebar on the website side is determined in a header in markdown files
- discussed about future changes to be included in the release after the next one:
  + unnamed model, Tiago is aiming for a mix: either a type has one unnamed model or it has multiple named model. The reason is that it avoids having to define Gospel types for complex models. The downside it that tools will have to handle both cases.
  + ownership (complete or no ownership) and spatial types
  + this should allow to completely remove coercion
