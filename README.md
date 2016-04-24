# autocompletion

A same problem viewed two ways: list-processing and graph-processing

> You are given a list of keywords below. Write code that will offer up to
  4 suggested “auto­complete” based on the letters typed (not case
  sensitive). Similar to Google Autocomplete, except that results must
  be in order vs. Google ranked keywords.

These few lines of codes are a proposal. They features two ways of
  achieving autocompletion: one first constructs a graph (called
  substrate) then queries it. The second one is dichotomy and sorting to
  achieve faster and more scalable results. The remaining lines of this
  file will elaborate on the algorithms and data structures used by both
  ways.
