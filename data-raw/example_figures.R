# example figures and table numbers for the paper templates
# run by hand from the project root; writes inst/templates/paper/figures/
# data: dplyr::starwars. the duel is drawn, not a film still (copyright)

library(dplyr)
library(ggplot2)

out <- file.path("inst", "templates", "paper", "figures")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
save <- function(plot, name) ggsave(file.path(out, name), plot, width = 6, height = 4)

sw <- starwars |> filter(!is.na(height), !is.na(mass), mass < 1000)   # drop Jabba
heroes <- c("Darth Vader", "Luke Skywalker")
theme_set(theme_minimal(base_size = 13))

# 1. the duel ------------------------------------------------------------------
fighter <- function(x, side, body, label) {
  # side = +1 faces right, -1 faces left
  segs <- data.frame(
    x    = c(x, x, x, x, x),
    y    = c(5.8, 3.8, 3.8, 5.4, 5.4),
    xend = c(x, x - 0.6, x + 0.6, x - side * 0.7, x + side * 0.9),
    yend = c(3.8, 2, 2, 4.6, 5.0)
  )
  list(
    geom_segment(data = segs, aes(x, y, xend = xend, yend = yend),
                 colour = body, linewidth = 3, lineend = "round"),
    annotate("point", x, 6.3, size = 14, colour = body),
    annotate("text", x, 1.4, label = label, colour = "grey85", size = 5)
  )
}
saber <- function(x, y, xend, yend, colour) list(
  annotate("segment", x = x, y = y, xend = xend, yend = yend,
           colour = colour, linewidth = 7, alpha = 0.3, lineend = "round"),   # glow
  annotate("segment", x = x, y = y, xend = xend, yend = yend,
           colour = "white", linewidth = 1.5, lineend = "round")              # core
)
set.seed(1977)
stars <- data.frame(x = runif(150, 0, 10), y = runif(150, 0, 8))

duel <- ggplot() +
  geom_point(data = stars, aes(x, y), colour = "white", size = 0.3, alpha = 0.7) +
  annotate("polygon", x = c(2.6, 3.4, 3.9, 2.1), y = c(5.8, 5.8, 2.2, 2.2),
           fill = "grey15") +                                                  # cape
  fighter(3, +1, "grey35", "Darth Vader") +
  fighter(7, -1, "wheat3", "Luke Skywalker") +
  saber(3.9, 5.0, 5.6, 7.3, "red") +
  saber(6.1, 5.0, 4.4, 7.3, "deepskyblue") +
  coord_fixed(xlim = c(0, 10), ylim = c(1, 8)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#0b0b1a", colour = NA))
save(duel, "example_duel.pdf")

# 2. height and mass ------------------------------------------------------------
height_mass <- ggplot(sw, aes(height, mass)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "grey40") +
  geom_point(alpha = 0.6) +
  geom_point(data = filter(sw, name %in% heroes), colour = "red", size = 3) +
  geom_text(data = filter(sw, name %in% heroes), aes(label = name),
            nudge_y = 12, colour = "red") +
  labs(x = "Height (cm)", y = "Mass (kg)")
save(height_mass, "example_height_mass.pdf")

# 3. three bar charts for the 2x2 grid ------------------------------------------
top_bar <- function(var, xlab) {
  starwars |>
    filter(!is.na({{ var }})) |>
    count({{ var }}, sort = TRUE) |>
    slice_head(n = 6) |>
    ggplot(aes(n, reorder({{ var }}, n))) +
    geom_col(fill = "grey30") +
    labs(x = xlab, y = NULL)
}
save(top_bar(species, "Characters"), "example_species.pdf")
save(top_bar(homeworld, "Characters"), "example_homeworld.pdf")

films <- starwars |>
  mutate(n_films = lengths(films)) |>
  slice_max(n_films, n = 6, with_ties = FALSE) |>
  ggplot(aes(n_films, reorder(name, n_films))) +
  geom_col(fill = "grey30") +
  labs(x = "Films", y = NULL)
save(films, "example_films.pdf")

# 4. numbers for the template tables --------------------------------------------
descriptives <- sw |>
  mutate(birth_year = ifelse(is.na(birth_year), NA, birth_year)) |>
  select(height, mass, birth_year) |>
  tidyr::pivot_longer(everything()) |>
  filter(!is.na(value)) |>
  group_by(name) |>
  summarise(N = n(), Mean = mean(value), SD = sd(value), Min = min(value), Max = max(value))
print(descriptives, width = Inf)

sw <- mutate(sw, human = species %in% "Human", masculine = gender %in% "masculine")
models <- list(
  lm(mass ~ height, sw),
  lm(mass ~ height + human, sw),
  lm(mass ~ height + human + masculine, sw)
)
for (m in models) {
  print(round(summary(m)$coefficients[, 1:2], 2))
  cat("N =", nobs(m), " R2 =", round(summary(m)$r.squared, 2), "\n\n")
}
