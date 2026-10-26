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

# height and mass ------------------------------------------------------------
height_mass <- ggplot(sw, aes(height, mass)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "grey40") +
  geom_point(alpha = 0.6) +
  geom_point(data = filter(sw, name %in% heroes), colour = "red", size = 3) +
  geom_text(data = filter(sw, name %in% heroes), aes(label = name),
            nudge_y = 12, colour = "red") +
  labs(x = "Height (cm)", y = "Mass (kg)") +
  theme_bw()

save(height_mass, "example_height_mass.pdf")

# three bar charts for the 2x2 grid ------------------------------------------
top_bar <- function(var, xlab) {
  starwars |>
    filter(!is.na({{ var }})) |>
    count({{ var }}, sort = TRUE) |>
    slice_head(n = 6) |>
    ggplot(aes(n, reorder({{ var }}, n))) +
    geom_col(fill = "grey30") +
    labs(x = xlab, y = NULL)+
    theme_bw()
}
save(top_bar(species, "Characters"), "example_species.pdf")
save(top_bar(homeworld, "Characters"), "example_homeworld.pdf")

films <- starwars |>
  mutate(n_films = lengths(films)) |>
  slice_max(n_films, n = 6, with_ties = FALSE) |>
  ggplot(aes(n_films, reorder(name, n_films))) +
  geom_col(fill = "grey30") +
  labs(x = "Films", y = NULL)+
  theme_bw()
save(films, "example_films.pdf")

# numbers for the template tables --------------------------------------------
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
