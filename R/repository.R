#' @importFrom purrr map_chr
parse_summary_issue <- function(x) {
  labels <- map_chr(x$labels$nodes, "name")

  list(p1 = x$p1$totalCount %||% 0, bugs = "bug" %in% labels, features = "feature" %in% labels, unlabeled = !length(labels))
}
get_repo_status <- function(x){
  readme <- x$README$text %||% character()
  path <- tempfile()
  writeLines(readme, path)
  badges <- codemetar::extract_badges(path)
  file.remove(path)
  status_badge <- badges[grepl("Project Status", badges$text)|
                           grepl("lifecycle", badges$text),]
  if(!is.null(status_badge)){
    glue::glue('<a href="{status_badge$link}"><img src="{status_badge$image_link}" alt="{status_badge$text}"></a>')
    
  }else{
    ""
  }
   
}
parse_summary_repository <- function(x) {
  tibble::tibble(
    owner = x$owner$login,
    repo = x$repo,
    prs = x$prs$totalCount,
    watchers = x$watchers$totalCount,
    open_issues = x$open_issues$totalCount,
    description = list(desc::desc(text = x$DESCRIPTION$text %||% character())),
    status = get_repo_status(x))
}

#' Compute an organization summary
#' @param org A GitHub user, either a normal user or an organization
#' @export
#' @importFrom dplyr group_by summarize left_join
#' @importFrom tidyr replace_na
org_data <- function(org) {
  res <- paginate(function(cursor, ...) graphql_query("repo_summary.graphql", org = org, cursor = cursor))

  summary <- map_dfr(res, function(x) map_dfr(x$repositoryOwner$repositories$nodes, parse_summary_repository))
  issues <- map_dfr(res, function(x) map_dfr(x$repositoryOwner$repositories$nodes, parse_issues_repository))

  # filter only packages
  repos <- unique(summary[, c("owner", "repo")])
  repos$is_pkg <- purrr::map2_lgl(repos$owner, repos$repo,
                                  ghrecipes::is_package_repo)
  summary <- left_join(summary, repos)
  summary <- summary[summary$is_pkg, ]
  summary <- dplyr::select(summary, - is_pkg)
  issues <- left_join(issues, repos)
  issues <- issues[issues$is_pkg, ]
  summ_issues1 <- issues %>%
    group_by(owner, repo) %>%
    summarize(p1 = sum(p1),
              unlabeled = sum(lengths(labels) == 0))
  summ_issues2 <- issues %>%
    group_by(owner, repo) %>%
    tidyr::unnest(labels) %>%
    dplyr::mutate(value = TRUE) %>%
    tidyr::spread(labels, value, fill = FALSE)
  summ_issues2 <- summ_issues2 %>%
    dplyr::summarise_at(names(summ_issues2)[8:ncol(summ_issues2)],
                        sum, na.rm = TRUE)
  
  summ_issues <- left_join(summ_issues1, summ_issues2)
  names(summ_issues) <- purrr::map_chr(names(summ_issues),
                                       emojify)
  
  if(nrow(summ_issues) > 0){
    summary <- left_join(
      summary,
      summ_issues %>%
        replace_na(list(p1 = 0, unlabeled = 0)))
  }
  

  list(summary = summary, issues = issues)
}
num_label <- function(x, label) {
  sum(map_lgl(x, ~ any(.x == label)))
}