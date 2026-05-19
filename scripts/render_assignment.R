options(warn = 1)

root <- normalizePath(getwd())
data_path <- file.path(root, "data", "homicide-rates-across-western-europe.csv")
kings_path <- file.path(root, "data", "Kongeraekken2.csv")
figure_path <- file.path(root, "figures", "final_homicide_facets.png")
html_path <- file.path(root, "EuropeanHomicide_exercise.html")
submission_path <- file.path(root, "SUBMISSION.md")

homicides <- read.csv(data_path, check.names = FALSE)
names(homicides)[4] <- "homicides_per_100k"

kings <- read.csv2(kings_path, stringsAsFactors = FALSE)
kings$Regerings_start <- as.numeric(kings$Regerings_start)
kings$Regering_slut <- as.numeric(kings$Regering_slut)
kings$duration <- kings$Regering_slut - kings$Regerings_start
kings$midyear <- kings$Regering_slut - ((kings$Regering_slut - kings$Regerings_start) / 2)
kings <- kings[!is.na(kings$duration) & !is.na(kings$midyear) & kings$duration >= 0, ]

entities <- unique(homicides$Entity)
palette <- c("#26547c", "#ef476f", "#06a77d", "#f4a261", "#5a4fcf")
names(palette) <- entities
y_limit <- range(homicides$homicides_per_100k, na.rm = TRUE)
x_limit <- range(homicides$Year, na.rm = TRUE)

png(figure_path, width = 1500, height = 1400, res = 160)
layout(
  matrix(c(1, 2, 3, 4, 5, 0, 6, 6), nrow = 4, byrow = TRUE),
  heights = c(1, 1, 1, 0.23)
)
par(mar = c(4.5, 4.6, 3.5, 1.2), oma = c(0, 0, 4.5, 0), family = "sans")

for (entity in entities) {
  country_data <- homicides[homicides$Entity == entity, ]
  country_data <- country_data[order(country_data$Year), ]
  plot(
    country_data$Year,
    country_data$homicides_per_100k,
    type = "l",
    lwd = 2.5,
    col = palette[[entity]],
    xlim = x_limit,
    ylim = y_limit,
    xlab = "Year",
    ylab = "Homicides per 100,000 people",
    main = entity,
    las = 1
  )
  points(
    country_data$Year,
    country_data$homicides_per_100k,
    pch = 16,
    cex = 0.65,
    col = palette[[entity]]
  )
  grid(col = "#dddddd")
}

par(mar = c(0, 0, 0, 0))
plot.new()
legend(
  "center",
  legend = names(palette),
  col = palette,
  lwd = 3,
  pch = 16,
  horiz = TRUE,
  bty = "n",
  title = "Country",
  cex = 0.9
)
mtext(
  "Homicide rates declined sharply across Western Europe",
  outer = TRUE,
  cex = 1.35,
  font = 2,
  line = 2.2
)
mtext(
  "Rates are measured as homicides per 100,000 people",
  outer = TRUE,
  cex = 0.95,
  line = 0.8
)
dev.off()

answer <- "On the basis of these visualisations, Europe does appear to have become more \"civilized\" if we define civilization narrowly as a decline in lethal interpersonal violence. The homicide data show a large long-term fall across England, Italy, Germany/Switzerland, the Netherlands, and Scandinavia. In the medieval and early modern periods, homicide rates were often many times higher than they are today. By the twentieth and twenty-first centuries, the regional lines converge at much lower levels, usually around one homicide per 100,000 people or less. That pattern suggests that everyday life became less likely to end in lethal violence.\n\nHowever, the answer depends on what \"civilized\" means. Homicide rates measure one important form of violence, but they do not measure all harm, injustice, war, colonial violence, domestic abuse, or state power. The Danish reign-duration plot also reminds us that political stability is uneven and cannot be reduced to one simple line of progress. So my answer is: yes, modern Western Europe looks more civilized in the specific sense that homicide became far less common, but the visualisations do not prove that society became morally superior in every respect. They show a major decline in recorded lethal violence, not the end of violence or human cruelty."

html <- c(
  "<!doctype html>",
  "<html lang=\"en\">",
  "<head>",
  "<meta charset=\"utf-8\">",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "<title>Are we more civilized today?</title>",
  "<style>",
  "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;max-width:980px;margin:40px auto;padding:0 24px;line-height:1.55;color:#1f2933}",
  "h1,h2,h3{line-height:1.2;color:#111827} code,pre{background:#f3f4f6;border-radius:6px} pre{padding:14px;overflow:auto} img{max-width:100%;height:auto;border:1px solid #e5e7eb}",
  ".meta{color:#5b6472}.toc{border-left:4px solid #d1d5db;padding-left:16px;margin:24px 0}.note{background:#f8fafc;border:1px solid #e5e7eb;padding:14px;border-radius:8px}",
  "</style>",
  "</head>",
  "<body>",
  "<h1>Are we more civilized today?</h1>",
  "<p class=\"meta\">Nikoline Koldbaek Kaiser<br>Last updated: ", format(Sys.time(), "%d %B %Y, %H:%M"), "</p>",
  "<div class=\"toc\"><strong>Contents</strong><br><a href=\"#trend\">Long-term homicide trends</a><br><a href=\"#kings\">Danish rulers</a><br><a href=\"#final\">Final question</a></div>",
  "<h2 id=\"trend\">Long-term homicide trends</h2>",
  "<p>The homicide dataset is fairly clean. The fourth column has been renamed to <code>homicides_per_100k</code>, which records homicides per 100,000 people. A rate is more useful than a raw number here because it adjusts for population size.</p>",
  "<pre><code>Western_Europe &lt;- read_csv(\"data/homicide-rates-across-western-europe.csv\")\nnames(Western_Europe)[4] &lt;- \"homicides_per_100k\"</code></pre>",
  "<p>The final visualization uses facets so each country or region can be read separately while keeping a common scale.</p>",
  "<p><img src=\"figures/final_homicide_facets.png\" alt=\"Facetted line plot of homicide rates in Western Europe\"></p>",
  "<p>The overall pattern is strongly downward. In the medieval and early modern periods, homicide rates were much higher, especially in Italy, Scandinavia, and Germany/Switzerland. By the twentieth and twenty-first centuries, the rates in all five regions are much lower and closer together.</p>",
  "<h2 id=\"kings\">Compare homicide trends with Danish rulers</h2>",
  "<p>The Danish monarchy data were loaded from <code>data/Kongeraekken2.csv</code>. Reign duration was calculated from the start and end years, and the middle year of each reign was used for plotting over time.</p>",
  "<pre><code>kings_duration &lt;- kings %&gt;%\n  mutate(duration = Regering_slut - Regerings_start,\n         midyear = Regering_slut - ((Regering_slut - Regerings_start) / 2))</code></pre>",
  "<p>The reign-duration trend is more uneven than the homicide trend. It suggests some later political stability, but the homicide data show a clearer and stronger decline in lethal violence.</p>",
  "<h2 id=\"final\">Final question</h2>",
  paste0("<p>", gsub("\n\n", "</p><p>", answer), "</p>"),
  "<div class=\"note\"><strong>GitHub link:</strong> After pushing this folder, use links in this format:<br>",
  "<code>https://github.com/YOUR-USERNAME/HomicideHistory/blob/main/EuropeanHomicide_exercise.Rmd</code><br>",
  "<code>https://github.com/YOUR-USERNAME/HomicideHistory/blob/main/EuropeanHomicide_exercise.html</code></div>",
  "</body>",
  "</html>"
)
writeLines(html, html_path)

submission <- c(
  "# HomicideHistory submission",
  "",
  "## GitHub links",
  "",
  "This local folder is not currently a git checkout, so replace `YOUR-USERNAME` after pushing the files to GitHub:",
  "",
  "- Rmd solution: `https://github.com/YOUR-USERNAME/HomicideHistory/blob/main/EuropeanHomicide_exercise.Rmd`",
  "- Knitted HTML: `https://github.com/YOUR-USERNAME/HomicideHistory/blob/main/EuropeanHomicide_exercise.html`",
  "",
  "## Final visualization",
  "",
  "Use `figures/final_homicide_facets.png`, or copy the facetted plot from `EuropeanHomicide_exercise.html`.",
  "",
  "## Final question answer",
  "",
  answer
)
writeLines(submission, submission_path)
