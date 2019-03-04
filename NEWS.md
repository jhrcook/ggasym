# ggasym 0.0.0.9002

* The `asymmetrise` function was included to prepare the input table before handing to ggplot2. This was necessary after I realized my previous verion would not work if you passed the aesthetics for `x` and `y` to `ggplot()` before adding to `geom_asymmat()`.

# ggasym 0.0.0.9001

* I have figured out how to adjust the fill of the top-left and bottom-right triangles by adjusting the actual ggproto object. Therefore, I will be able to implement `ggasym` as a natural ggplot2 `geom_`! Currently, only `scale_fill_tl/br_fill()` are functional, but are great proofs-of-concept.

# ggasym 0.0.0.9000

* `pkgdown` website began - `ggasym` not yet functional.
