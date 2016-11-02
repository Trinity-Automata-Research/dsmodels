Authors: Seth Fogarty,          Charles Stein
Contact: sfogarty@trinity.edu,  cstein1@trinity.edu

dsmodels is an expressive language to facilitate the creation and visualization
    of two-dimensional dynamical systems. The basic elements of the language are
    a model wrapping around a function(x,y) which outputs a list(x
    = xprime, y = yprime), and a range. The language supports three
    types of visual objects: visualizations, features, and backgrounds. Visualizations, including dots and arrows,
    depict the behavior of the dynamical system over the entire range.
    Features display
    user-defined curves and points, and their images under the system.
    Backgrounds define and color regions of interest, such as areas of convergence and divergence.
    The language
    can also automatically guess attractors and regions of convergence and divergence.

To install dsmodels, simply type into your R console the following line:
     `install.packages("dsmodels")`.

Place the following command as the first line of your script to begin using dsmodels:
     `library(dsmodels)`
     
To uninstall dsmodels, simply run the following command:
     `remove.packages("dsmodels")`


To see examples of dsmodels, check the Examples folder.
