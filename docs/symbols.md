# All About the Symbol Table

| `symbolkind::` | prefix in C | comdat | address | in an internal | core size | index means | type | shape |
| -------------- | ----------- | ------ | ------- | -------------- | --------- | ----------- | ---- | ----- |
| `local` | `v` |  | yes | alias parent | actual |  | actual | allowed |
| `common` | `v` | block | yes | alias parent | actual | order in block | actual | allowed |
| `argument` | `v` |  | no | unique | 0 words | position in call | actual | allowed |
| `retval` | `v` |  | yes | unique | actual |  | actual | no? |
| `temporary` | `vtmp` |  | yes | unique | 1 word |  | INTEGER (hack) | no |
| `subprogram` | `sub` or `fn` |  | no | alias parent | 0 words | # of args | actual iff func | no |
| `internal` | `fn` |  | no | alias parent | 0 words | # of args | actual iff func | no |
| `external` | `sub` (for now) |  | yes (for ptr) | not allowed | 1 word |  | subptr | no |
| `label` | `L` |  | no | not allowed | 0 words |  | none | no |
| `format` | `fmt` |  | no (for now) | not allowed | 0 words |  | none | no |







Considering: |  |  |  |  |  |  |  | 
Rename `label` to `target` because these symbols represent only branch targets. |  |  |  |  |  |  |  | 
Someday, we might want format strings in core, requiring an address and a size. |  |  |  |  |  |  |  | 
Use a table of C function pointers (outside of core) and change external subptrs to indices into that table. |  |  |  |  |  |  |  | 
Overload comdat for name of parent for internals. |  |  |  |  |  |  |  | 
Allow external to be functions as well as subroutines. |  |  |  |  |  |  |  | 
