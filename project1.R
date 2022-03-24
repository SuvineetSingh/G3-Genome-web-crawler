require(rvest)
top_level_url <- 'https://academic.oup.com'
input_year <- '2022'
base_url <- 'https://academic.oup.com/g3journal/issue-archive'
html <- read_html(base_url)
years <- html %>% html_elements(".widget-IssueYears div")
year_issue_links <- c()
year_issue_names <- c()
for (year in years) {
  link <- html_element(year, 'a') %>% html_attr('href')
  name <- html_element(year, 'a') %>% html_text()
  year_issue_links <- c(year_issue_links, link)
  year_issue_names <- c(year_issue_names, name)
}
names(year_issue_links) <- year_issue_names
show(year_issue_links)

input_year_link <- paste(top_level_url, year_issue_links[input_year], sep='')
year_html <- read_html(input_year_link)

issue_links <- year_html %>% html_elements('.widget-IssuesAndVolumeListManifest ul div a') %>% html_attr('href')
article_links <- c()
for (link in issue_links) {
  issue_link <- paste(top_level_url, link, sep='')
  issue_html <- read_html(issue_link)
  current_article_links <- issue_html %>% html_elements('.section-container section .al-article-list-group .al-article-item-wrap .al-article-items .item-title a') %>% html_attr('href')
  article_links <- c(article_links, current_article_links)
}
print(length(article_links))

headers <- c('Title', 'Authors', 'Author Affiliations', 'Correspondence Author', 'Correspondence Author Email', 'Published Date', 'Abstract', 'Keywords', 'Full Paper')
final_df <- data.frame(matrix(ncol = length(headers), nrow = 0))
for (link in article_links) {
  print(link)
  article_link <- paste(top_level_url, link, sep='')
  article_html <- read_html(article_link)
  article_title <- article_html %>% html_element('.wi-article-title') %>% html_text(trim = TRUE)
  article_authors <- article_html %>% html_elements('.al-authors-list .linked-name') %>% html_text()
  article_author_affiliations <- article_html %>% html_elements('.info-card-affilitation') %>% html_text(trim = TRUE)
  info_card_author <- article_html %>% html_elements('.info-card-author')
  for (card in info_card_author) {
    t <- card %>% html_element('.info-author-correspondence')
    if (class(t) == 'xml_node') {
      article_author_correspondence <- card %>% html_elements('.name-role-wrap') %>% html_text(trim = TRUE)
    }
    else {
      article_author_correspondence <- NA
    }
  }
  article_author_correspondence_email <- article_html %>% html_elements('.info-author-correspondence a') %>% html_text(trim = TRUE)
  article_published <- article_html %>% html_element('.citation-date') %>% html_text2()
  article_abstract <- article_html %>% html_element('.abstract') %>% html_text2()
  article_keywords <- article_html %>% html_element('.kwd-group') %>% html_text2()
  article_paper <- article_html %>% html_elements('.chapter-para') %>% html_text2()
  current_row <- c(
    paste(article_title, collapse = ', '),
    paste(article_authors, collapse = ', '),
    paste(article_author_affiliations, collapse = ', '),
    paste(article_author_correspondence, collapse = ', '),
    paste(article_author_correspondence_email, collapse = ', '),
    paste(article_published, collapse = ', '),
    paste(article_abstract, collapse = ', '),
    paste(article_keywords, collapse = ', '),
    paste(article_paper, collapse = ', ')
  )
  final_df <- rbind(final_df, current_row)
}

names(final_df) <- headers
print(dim(final_df))
write.csv(x=final_df, file='project.csv')

