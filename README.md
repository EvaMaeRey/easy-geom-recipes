

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Easy geom recipes: a new entry point to ggplot2 extension for statistical educators and their students

<!-- badges: start -->
<!-- badges: end -->
<!-- Informal background:  -->
<!-- *My entry to ggplot2 extension was slow.  I’d say it took about a year – all of 2020 -- going back and forth between materials (Thomas Lin Pederson 'extending your ability to extend' RStudio presentation), ggplot2 extension vignette (part of the ggplot2 package), and the extension section of the new edition of the ggplot2 book to find a path to the geom I was after.  But once I got going, I found that I used one that mechanisms over and over. It is the mechanims that is featured in the easy recipes tutorial: compute_group. I think it is a cool entry point; for mathminded folks --- maybe more exciting than defining themes which is often the promoted entry point? In Fall 2021, I took on two students to do independent studies to create additional geoms.  Working with them really helped define the steps of the process. And in the following spring term, one of the students stayed on and helped craft the compute_group ‘recipes’ with me.* -->

# Study material

- [survey_results_summary.html](https://evamaerey.github.io/easy-geom-recipes/survey_results_summary.html)
- [survey_instrument.html](https://evamaerey.github.io/easy-geom-recipes/survey_instrument.html)
- [invitation_to_participate.html](https://evamaerey.github.io/easy-geom-recipes/invitation_to_participate.html)
- [index.html](https://evamaerey.github.io/easy-geom-recipes/index.html)
- [focus_group_script.html](https://evamaerey.github.io/easy-geom-recipes/focus_group_script.html)
- [focus_group_highlights.html](https://evamaerey.github.io/easy-geom-recipes/focus_group_highlights.html)
- [easy_geom_recipes_compute_panel.html](https://evamaerey.github.io/easy-geom-recipes/easy_geom_recipes_compute_panel.html)
- [easy_geom_recipes_compute_group.html](https://evamaerey.github.io/easy-geom-recipes/easy_geom_recipes_compute_group.html)
- [easy_geom_recipes.html](https://evamaerey.github.io/easy-geom-recipes/easy_geom_recipes.html)
- [about.html](https://evamaerey.github.io/easy-geom-recipes/about.html)
- [2023_easy_geom_recipes.html](https://evamaerey.github.io/easy-geom-recipes/2023_easy_geom_recipes.html)

### Abstract

This paper introduces a new introductory tutorial in ggplot2 extension.
The tutorial explores six layer extensions in a step-by-step fashion.
Three of the extensions are fully-worked examples. After each of the
worked examples, the tutorial prompts the learner to work through a
similar extension.

The tutorial was evaluated by statistics and data analytics educators
with substantial R and ggplot2 experience, but without much extension
experience.

In general, the tutorial was found to be engaging and appropriate in
length. The level was judged to be accessible, such that instructors
would feel comfortable sharing with advanced students.

### Narrative

Using ggplot2 has been described as writing ‘graphical poems’. But we
may feel at a loss for ‘words’ when functions we’d like to have don’t
exist. The ggplot2 extension system allows us to build new ‘vocabulary’
for fluent expression.

An exciting extension mechanism is that of inheriting from existing,
more primitive geoms after performing some calculation.

To get feet wet in this world and provide a taste of patterns for geom
extension, ‘easy geom recipes’ provide three basic examples of the
geoms\_ that inherit from *existing* geoms (point, text, segment, etc)
along with a practice exercise:
https://evamaerey.github.io/easy-geom-recipes/easy_geom_recipes_compute_group.html.
With such geoms, calculation is done under the hood by the ggplot2
system. We’ll see how to define a ggproto object; The tutorial keeps
things simple by only defining computation at the compute_group stage.

With new geoms, you can write *new* graphical poems with exciting new
graphical ‘words’!

The tutorial is intended for individuals who already have a working
knowledge of the grammar of ggplot2, but may like to build a richer
vocabulary for themselves.

### Motivation

- ggplot2 extension underutilized

- inaccessibility to demographic that might really benefit

- statistical storytelling benefits from greater fluidity that new geoms
  afford

- traditional entry point (themes) might not be compelling to an
  audience that might be really productive in extension!

- repetition and numerous examples useful in pedegogical materials

### Tutorial form

The introductory tutorial took the form of three examples where a
geom\_\* is successfully created in a step-by-step fashion. After each
example, the learner is prompted to create an additional geom that is
computationally similar to the example.

Our recipes take the form:

- Step 0. Get the job done with ‘base’ ggplot2. It’s a good idea to
  clarify what needs to happen without getting into the extension
  architecture
- Step 1. Write a computation function. Wrap the necessary computation
  into a function that your target geom\_\*() function will perform. We
  focus on ‘compute_group’ computation only in this tutorial.
- Step 2. Define a ggproto object. ggproto objects allow your extension
  to work together with base ggplot2 functions! You’ll use the
  computation function from step 1 to help define it.
- Step 3. Write your geom function! You’re ready to write your function.
  You will incorporate the ggproto from step 2 and also define which
  more primitive geom (point, text, segment etc) you want other
  behaviors to inherit from.
- Step 4. Test/Enjoy! Take your new geom for a spin! Check out
  group-wise computation behavior!

## Evaluation

To test the tutorial, we approached statistics and data analytics
educators that we believed would have substantial experience with R and
ggplot2 but not necessarily with ggplot2 extension. The study includes
nine participants that completed the study and responded to a survey
about the tutorial. Eight of the participants were also able to
participate in focus group discussions following their completion of the
the tutorial and survey.

### Participant profiles

Recruited participants generally had substantial experience teaching
data analytics.

![](survey_results_summary_files/figure-html/unnamed-chunk-7-1.png)

Study participants all had substantial experience. All had at least five
years programming in the R statistical language, with the majority
having more than ten years of experience.

<img
src="survey_results_summary_files/figure-html/q05r_length_user-1.png"
style="width:50.0%" alt="hello" />

Most of the participants identified as frequent users of R, using the
language almost every day.

![hello](survey_results_summary_files/figure-html/q06r_frequency-1.png)

Furthermore, most of the participants (7 of 9) responded that they use
ggplot2 several times a week or more.

![hello](survey_results_summary_files/figure-html/q07ggplot2_frequency-1.png)

With respect to writing functions, most of the group had experience
writing functions, though the frequency was not as great as with using R
and ggplot2.

![](survey_results_summary_files/figure-html/q08r_frequency-1.png)

The respondents use ggplot2 in a variety of contexts; notably they all
use it in academic research and eight of nine used it in teaching.

![](survey_results_summary_files/figure-html/q09ggplot2_contexts-1.png)

However, participants in general had little or no with writing ggplot2
extensions. Seven of the nine participants were aware of extension
packages but had not attempted to write their own extension. One
participant had written ggplot2 extensions prior to the tutorial.

![](survey_results_summary_files/figure-html/q10_previous_ext_experience-1.png)

The following figure shows attempts and successes or failures in the
different ggplot2 extension areas.

![](survey_results_summary_files/figure-html/q11_previous_ext_attempt-1.png)

The participants had a variety of experiences with object oriented
programming, but the majority had no experience with object oriented
programming in R. (confirm this w/ underlying data because looking at
the summary figure, we can’t 100% confirm this. But I think it’s true.)

![](survey_results_summary_files/figure-html/q13_oop_experience-1.png)

# Tutorial assessment

Most participants indicated the tutorial taking them a short amount of
time. Six of nine said that on average, each of the recipes took less
than 15 minutes to complete. The remaining three participants responded
that the recipes took between 15 and 30 minutes on average.

![](survey_results_summary_files/figure-html/q16tutorial_time_taken-1.png)

The first prompt `geom_point_xy_means` exercise was completed by all
participants; and all but one completed the `geom_text_coordinates()`
exercise. Several participants failed complete the last recipe
(residuals).

![](survey_results_summary_files/figure-html/q14which_completed-1.png)

![](survey_results_summary_files/figure-html/q17tutorial_length-1.png)

![](survey_results_summary_files/figure-html/q18example_clarity-1.png)

![](survey_results_summary_files/figure-html/q19examples_engaging-1.png)

![](survey_results_summary_files/figure-html/unnamed-chunk-4-1.png)

![](survey_results_summary_files/figure-html/unnamed-chunk-5-1.png)

------------------------------------------------------------------------

![](survey_results_summary_files/figure-html/unnamed-chunk-6-1.png)

### Focus group highlights…

#### mechanics

> ’… going through this was super helpful cuz now I like understand the
> mechanics of it all.

> ‘And so I don’t have any intentions of like making formal geoms on my
> own for anything yet. But it was like really helpful for understanding
> how the whole system works.’

#### failure w/ previous attempts

> ‘So like there’s some other layer of getting into the ggplot extension
> world that I \[was\] missing.’

#### Step by step and step 0

> ’So pedagogically, I liked how it was. I like the the general like
> steps like start with like make the GEOM manually with regular ggplot
> and Step 0 just to have like a baseline and then going to each of the
> steps to get there and then being able to compare with the original
> like as far as like pedagogically, that was super helpful. Just as as
> an approach to to get it right just so you can have a goal and see how
> all of these, these different primitives and proto elements and
> whatever fit together… in such done that really helpful.

# Skepticism

> And it was that easy. And I felt empowered as a result of that…. But
> you know, like, my problem isn’t gonna be that easy.

# Concern - missing values

> When, like place where you might have an opportunity to do a little
> bit of pedagogical caution, it’s with like missing values. So when
> computing a mean like ggplot says, by the way, there were three rows I
> didn’t plot…. So then you need to explicitly override the default and
> that something like that so that people are not just blindly putting
> summaries down without considering the data that are being used to
> make them.

# Accessibility for students

> I’m teaching, so I’m teaching data visualization this summer online
> again with my regular like online classes I’ve assigning \[the
> tutorial\]… as kind of like an extra credit thing at the end of
> semester to saying like, if you’re interested, go through this thing
> and you get 10 bonus points or something just for the more advanced
> students that will be in the class that will be interested. *But I
> think it’s totally accessible for them.*

# Higher level objectives

> I’ll just add that I I think we, I could definitely use materials like
> this and it did raise for me the points since I didn’t have a lot of
> experience writing extensions before like it was ended up being very
> comprehensible to me and so it kind of made me think that in like a
> data visuals, data visualization classes that we teach, we probably
> need to. There’s a balance between teaching the students to use the
> tools that exist right now to like, just do your analysis and just do
> the best with the tools that are available. But we probably should
> include a couple of weeks on like. You know, programming and writing
> extensions along these lines, because it’s obviously like very
> powerful and they need to have at least some exposure to it. So you
> know, a week or two weeks of materials kind of like this would be
> helpful and would help distinguish them from like being able to just
> work through tutorials on your own online. Like, if they could write
> their own extension, that’s like real value added, you know, to their
> organization. So it it made me think that I need to think about my
> data visualization class A little bit more as a programming class in
> some ways. And I thought that would be a good. You know, this is
> pretty good material along those lines.

# Relationship to writing functions

> Trying to clarify a little bit more when it’s Useful to have Your own
> costume Geom, as opposed to your own function

### Appendix, example exercise

For clarity, I include one of the three exercises in the ‘easy geom
recipes’ extension tutorial. First an ‘example recipe’ `geom_label_id()`
is provided, with the step 0-4 guideposts. Then, the student is prompted
to create `geom_text_coordinates()`.

```` default

# Example recipe #2:  `geom_label_id()`

---

## Step 0: use base ggplot2 to get the job done

```{r cars}
# step 0.a
cars %>% 
  mutate(id_number = 1:n()) %>% 
  ggplot() + 
  aes(x = speed, y = dist) + 
  geom_point() + 
  geom_label(aes(label = id_number), 
             hjust = 1.2)

# step 0.b
layer_data(last_plot(), i = 2) %>% 
  head()
```

---

## Step 1: computation

```{r compute_group_row_number}
# you won't use the scales argument, but ggplot will later
compute_group_row_number <- function(data, scales){
  
  data %>% 
    # add an additional column called label
    # the geom we inherit from requires the label aesthetic
    mutate(label = 1:n())
  
}

# step 1b test the computation function 
cars %>% 
  # input must have required aesthetic inputs as columns
  rename(x = speed, y = dist) %>% 
  compute_group_row_number() %>% 
  head()
```

---

## Step 2: define ggproto

```{r StatRownumber}
StatRownumber <- ggplot2::ggproto(`_class` = "StatRownumber",
                                  `_inherit` = ggplot2::Stat,
                                  required_aes = c("x", "y"),
                                  compute_group = compute_group_row_number)
```


---

## Step 3: define geom_* function



- define the stat and geom for your layer

```{r geom_label_row_number}
geom_label_row_number <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatRownumber, # proto object from Step 2
    geom = ggplot2::GeomLabel, # inherit other behavior, this time Label
    data = data, 
    mapping = mapping,
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
```





---

## Step 4: Enjoy! Use your function

```{r enjoy_again}
cars %>% 
  ggplot() + 
  aes(x = speed, y = dist) + 
  geom_point() + 
  geom_label_row_number(hjust = 1.2) # function in action
```

### And check out conditionality!

```{r conditional_compute}
last_plot() + 
  aes(color = dist > 60) # Computation is within group
```




---

# Task #2: create `geom_text_coordinates()`

Using recipe #2 as a reference, can you create the function `geom_text_coordinates()`.  

--

- geom should label point with its coordinates '(x, y)'
- geom should have behavior of geom_text (not geom_label)


Hint:

```{r}
paste0("(", 1, ", ",3., ")")
```

```{r}
# step 0: use base ggplot2

# step 1: write your compute_group function (and test)

# step 2: write ggproto with compute_group as an input

# step 3: write your geom_*() function with ggproto as an input

# step 4: enjoy!

```
````

Thanks to Claus Wilke, June Choe, Teun Van der Brand, Isabella
Velasquez, Cosima Meyer, and Eric Reder for pre-testing and reviewing
the tutorial and providing useful feedback.
