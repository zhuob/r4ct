---
title: "Package MethodDev"
author: "Bin Zhuo"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

## 
The function `rand_arm` is developed to generate randomized arm for trial 
simulation. It's designed to randomize arms for a given number of subjects `nsbj`
and allocation ratio `ratio`. 
