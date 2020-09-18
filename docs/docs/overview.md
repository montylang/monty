---
id: overview
title: Overview
slug: /
---

## Motivation

Monty is designed to introduce powerful functional features/abstractions in a familiar form. Learning
a language like Haskell with no prior functional experience can seem very daunting. Monty aims to
incorperate pythonic syntax with a subset of features and ideas usually found in languages like Haskell.
This hopefully encourages more people to get their feet wet and learn some functional basics. Hopefully
after some experienc with the basics Haskell and other "intimidating" languages will seem much more approachable.


## Core Ideas

While we wanted this language to feel similar to python we felt quite strongly that it should be a pure 
language. We've done our best to reuse python syntax to evoke a general intuition about how a monty feature
should be used. For example we use the keyword `class` in monty. In no way is monty object oriented, `class`
is actually used to define new data types. However there is some similarity between an object and a data type.
They're both containers that can hold some data. They also both have constructors that are used to
instantiate new instances using some parameters. We hope that by drawing parallels for new users the language
will be easier to pick up and learn.

This doesn't mean that there's no new syntax. We only use existing syntax/keywords when we feel it draws
a good parallel that will be helpful for new users. In cases where it doesn't new syntax will be introduced
as appropriate. In addition, while we've tried to keep as many language features from python as possible
there are some that needed to be modified in order for monty to be a cohesive functional language.
