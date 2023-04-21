
<!-- README.md is generated from README.Rmd. Please edit that file -->

# easy-geom-recipes

<!-- badges: start -->

<!-- badges: end -->

The goal of easy-geom-recipes is to provide points of entry into the
ggplot2 extension space, focusing on layers.

Informal background: *My entry to ggplot2 extension was slow. I’d say it
took about a year – all of 2020 – going back and forth between materials
(Thomas Lin Pederson ‘extending your ability to extend’ RStudio
presentation), ggplot2 extension vignette (part of the ggplot2 package),
and the extension section of the new edition of the ggplot2 book to find
a path to the geom I was after. But once I got going, I found that I
used one that mechanisms over and over. It is the mechanims that is
featured in the easy recipes tutorial: compute\_group. I think it is a
cool entry point; for mathminded folks — maybe more exciting than
defining themes which is often the promoted entry point? In Fall 2021, I
took on two students to do independent studies to create additional
geoms. Working with them really helped define the steps of the process.
And in the following spring term, one of the students stayed on and
helped craft the compute\_group ‘recipes’ with me.*

  - [survey\_results\_summary.html](https://evamaerey.github.io/easy-geom-recipes/survey_results_summary.html)
  - [survey\_instrument.html](https://evamaerey.github.io/easy-geom-recipes/survey_instrument.html)
  - [invitation\_to\_participate.html](https://evamaerey.github.io/easy-geom-recipes/invitation_to_participate.html)
  - [easy\_geom\_recipes\_compute\_panel.html](https://evamaerey.github.io/easy-geom-recipes/easy_geom_recipes_compute_panel.html)
  - [easy\_geom\_recipes\_compute\_group.html](https://evamaerey.github.io/easy-geom-recipes/easy_geom_recipes_compute_group.html)
  - [easy\_geom\_recipes.html](https://evamaerey.github.io/easy-geom-recipes/easy_geom_recipes.html)

I’ll also write the paper here.

-----

Using ggplot2 has been described as writing ‘graphical poems’. But we
may feel at a loss for ‘words’ when functions we’d like to have don’t
exist. The ggplot2 extension system allows us to build new ‘vocabulary’
for fluent expression.

An exciting extension mechanism is that of inheriting from existing,
more primitive geoms after performing some calculation.

To get your feet wet in this world and give you a taste of patterns for
geom extension, we provide three basic examples of the geoms\_ that
inherit from *existing* geoms (point, text, segment, etc) along with a
practice exercise. With such geoms, calculation is done under the hood
by the ggplot2 system. We’ll see how to define a ggproto object; in this
tutorial we’ll keep things simple by only defining computation at the
compute\_group stage.

With new geom, you can write *new* graphical poems with exciting new
graphical ‘words’\!

This tutorial is intended for individuals who already have a working
knowledge of the grammar of ggplot2, but may like to build a richer
vocabulary for themselves.
