# https://github.com/SometimesY/CrootBot/blob/master/getRivals.py
strip_suffix <- function(name) {
    name <- trim(name)
    result <- case_when(
        grepl(' Jr$', name) ~ gsub("Jr","", name),
        grepl(' Jr\\.$', name) ~ gsub("Jr.","", name),
        grepl(' II$', name) ~ gsub("II","", name),
        grepl(' III$', name) ~ gsub("III","", name),
        grepl(' IV$', name) ~ gsub("IV","", name),
        grepl(' V$', name) ~ gsub("V","", name),
        grepl(' VI$', name) ~ gsub("VI","", name),
        grepl(' VII$', name) ~ gsub("VII","", name),
        TRUE ~ trim(name)
    )
    return(result)
}

fixHeight <- function(x) paste0(floor(x/12),"-",x-(floor(x/12)*12))

trim <- function (x) gsub("^\\s+|\\s+$", "", gsub("^\\t*|\\t*$", "", x))
column <- function(x, css) x %>% html_node(css = css) %>% html_text()

splitJoin <- function(input) {
    tmp <- as.list(strsplit(as.character(input), split="\\s")[[1]]) %>%
        trim() %>%
        map(., ~ discard(.x, ~ nchar(.x) == 0)) %>%
        compact() %>%
        paste(sep=" ", collapse=" ")
    return(tmp)
}
