* write bytepatch exec
  * provide a handle of schemas, mainly bin strings, ASM
  * also provide the hash check

## Thoughts
### Check data is useful for many different things
A big plus is that it's useful for placeholders. I did this with GTVM, generated
a file of all UTF-8 strings (over a given length) then curated the interesting
ones and generated a placeholder patchscript.

But that's kind of not really the main use...? Not sure. I definitely want to
support it though.

## Linearizing
Two types:

* cursor follows after patch data
* cursor is independent from patch data (permits overwriting)

The latter seems much less useful.
