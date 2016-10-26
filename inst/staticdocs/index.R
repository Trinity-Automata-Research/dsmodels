sd_section("Foundation",
  "These objects are completely necessary for constructing a system using dsmodels",
  c(
    "dsmodel",
    "dsrange"
  )
)

sd_section("Visualizations",
  "These objects are used to visualize the system's behavior over the entire range",
  c(
    "dsdots",
    "dsarrows"
  )
)

sd_section("Features",
  "These objects are used to visualize behaviors of specific regions of the system",
  c(
    "dspoint",
    "dscurve",
	"dsregion",
	"xlabel",
	"ylabel"
  )
)

sd_section("Feature Guessing",
           "These objects are used to guess, with no guarantee whatsoever, the features of the model.",
           c(
             "guessattractors",
             "guessregions"
           ))
