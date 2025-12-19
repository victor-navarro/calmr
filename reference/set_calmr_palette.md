# Get/set the colour/fill palette for plots

Get/set the colour/fill palette for plots

## Usage

``` r
set_calmr_palette(palette = NULL)
```

## Arguments

- palette:

  A string specifying the available palettes. If NULL, returns available
  palettes.

## Value

The old palette (invisibly) if palette is not NULL. Otherwise, a
character vector of available palettes.

## Note

Changes here do not affect the palette used in graphs.
