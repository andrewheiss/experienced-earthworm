make_adobe_swatches <- function(out_file) {
  clrs <- MoMAColors::moma.colors("ustwo")
  
  clrs |> 
    set_names(paste("ustwo", 1:length(clrs))) |> 
    swatches::hex_to_ase("spot") |> 
    swatches::ase_encode() |> 
    writeBin(out_file)
  
  return(out_file)
}
