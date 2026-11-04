# Clean Class Names

Cleans class names, assigns default class information to students
without a class, and identifies classes containing identical students
within the same school.

## Usage

``` r
clean_class_names(data, default_class_name = "Reception")
```

## Arguments

- data:

  A class list dataframe.

- default_class_name:

  Default name for missing or merged classes. Defaults to "Reception".

## Value

A dataframe with additional configured columns CLASS_ID and CLASS_NAME.
