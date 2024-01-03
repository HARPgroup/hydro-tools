om_multi_image_list <- function(ds,model_pids, scenarios, image_names, column_descriptions = c(), label_prefix = "Image") {
  # gets the same image name from multiple models in a single scenario
  # puts into list format compatible with the fn om_rmd_img_table()
  thiscol = 1
  thisrow = 1
  img_list = list() 
  if (!(length(image_names) > 0)) {
    return(img_list)
  }
  n = 1
  for (i in 1:length(image_names)) {
    img_name = image_names[i]
    if (i > length(column_descriptions)) {
      img_desc = ""
    } else {
      img_desc = column_descriptions[i]
    }
    for (s in 1:length(scenarios)) {
      scenario = scenarios[s]
      for (m in 1:length(model_pids)) {
        model = ds$prop_json_cache[model_pids[[m]]]
        if (is.null(model[[1]])) {
          # try to retrieve?
          model <- list(name=paste("Unknown Model (note: you must run ds$get_json_prop() to retrieve models before running this function).", model_pids[[m]]))
        } else {
          model = model[[1]]
        }
        scen_results <- find_name(model, scenario)
        fig_prop <- find_name(scen_results,img_name)
        short_name <- find_name(scen_results, "short_name")
        img_list[[n]] = list()
        img_list[[n]]$text = paste(img_desc, model$name, short_name)
        img_list[[n]]$label = label_prefix
        if (!(is.null(fig_prop))) {
          fig_path <- fig_prop$code
          # we have an image, show it 
          img_list[[n]]$img_url = fig_path
          
        } else {
          # tbd: try impoundment linked to river segment.
          img_list[[n]]$text = paste("No property found for",model$name)
        }
        n = n + 1 # increment image counter
      }
    }
  }
  return(img_list)
}

om_rmd_img_table <- function(image_info, col_max=2, num_prefix="", num_delim=".") {
  thiscol = 1
  thisrow = 1
  if (num_prefix == "") {
    num_delim = ""
  }
  img_pct = round(100 / col_max)
  attribute_matrix <- matrix(NA, ceiling(length(image_info)/col_max),col_max)
  if (length(image_info) == 0) {
    return(attribute_matrix)
  }
  for (i in 1:length(image_info)) {
    this_info = image_info[[i]]
    image_path = this_info$img_url
    # sizing images in Rmd: https://bookdown.org/yihui/rmarkdown-cookbook/figure-size.html
    img_markdown <- paste(
      paste0("**", this_info$label, " ", paste0(num_prefix,num_delim,i), ":** ", this_info$text),
      paste0("![](",image_path,")","{width=",img_pct,"%}"),
      sep="\n"
    )
    # can as_image(), from flextable, be used to re-size?
    # do we need to know the page width, since these 
    # height and width are in inches, cm or mm?
    #width_in = img_pct * 6.5 / 100.0
    #height_in = img_pct * 9.0 / 100.0
    #img_markdown <- as_paragraph(as_image(image_path, width = width_in, height = height_in, guess_size = FALSE))
    if (thiscol > col_max) {
      thiscol = 1
      thisrow <- thisrow + 1
    }
    attribute_matrix[thisrow, thiscol] = img_markdown
    thiscol <- thiscol + 1
  }
  return(attribute_matrix)
}
