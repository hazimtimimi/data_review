# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot multiple graphs (faceted by country) to PDF split up into multiple pages
# The default is 16 graphs per page so that graphs are easy to view.
# This is necesary because we have over 200 countries and territories to plot.
# Hazim Timimi June 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

plot_blocks_to_pdf <- function(df,
                               countries,
                               pdf_file_name,
                               plot_function = plot_faceted,
                               block_size = 16 ) {

  # I want to produce graphs in blocks of 16 countries
  # The subset of the first 16 countries would be
  #
  # cc <- changes[as.character(changes$country) %in% countries[1:16,],]
  #
  # and the next block of 16 would be
  #
  # cc <- changes[as.character(changes$country) %in% countries[17:32,],]
  #
  # Remember 17:32 = 1*16 + 1 : 2*16 = (2 - 1)*16 + 1: 2*16
  # So the following code is generic (can change block size from the default of 16)
  # and uses a loop first to get plots for complete pages and finally mops up any
  # countries left unplotted that do not make up a complete page.

  full_blocks <- floor(nrow(countries)/block_size)

  # Open a PDF for output
  pdf(pdf_file_name)

  #print the full blocks
  for (block in 1:full_blocks) {
    plot_function(df[as.character(df$country)
                              %in%
                                countries[((block - 1)*block_size + 1):(block*block_size),], ])
  }

  # and now print the last few (incomplete block)
  if (full_blocks*block_size < nrow(countries)){
    plot_function(df[as.character(df$country)
                              %in%
                                countries[(full_blocks*block_size + 1):nrow(countries),], ])
  }

  # Save and close the PDF file
  dev.off()

}