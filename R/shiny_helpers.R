#' Some functions to help the shiny application.
#' None of these are exported to the user.
#' @param plots A list of plots
#' @param selection A character vector with the selected plots
#' @param options A character vector with all available plots
#' @import patchwork
#' @name shiny_helpers

#' @rdname shiny_helpers
patchPlots <- function(plots, selection, options){
  patch = ggplot2::ggplot()
  selected = length(selection)
  if (selected){
    #if we want common scales
    if (options$common_scale & selected > 1){
      #get min and max y-scale
      ranges = unlist(lapply(plots[selection], function(p) ggplot2::layer_scales(p)$y$range$range))
      miny = min(ranges)
      maxy = max(ranges)
      for (p in selection[1:selected]){
        plots[[p]] = plots[[p]] + ggplot2::coord_cartesian(ylim = c(miny, maxy))
      }
    }

    patch = plots[[selection[1]]]
    if (selected > 1){
      for (p in selection[2:selected]){
        patch = patch + plots[p]
      }
    }
  }
  return(patch + patchwork::plot_layout(guides = 'collect'))
}
