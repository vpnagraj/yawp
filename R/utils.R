## borrow helpers from patchwork for gg_zoom()
combine_plots <- patchwork:::`&.gg`
is_patchwork <- patchwork:::is_patchwork

## bindings for global variables
utils::globalVariables(c(".", "n", "sd", "se", "xmax", "xmin", "ymax", "ymin"))
