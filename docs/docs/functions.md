---
id: functions
title: Functions
---

## Python vs Monty

Let's start by taking a look at how functions are declared in python.

```python
def add1(x):
  return x + 1
```

This creates a function named `add1` that takes a single parameter (who's value
will be bound to `x` and returns `x + 1`. Pretty simple. Now let's take a look at what 
that same function looks like in monty.

```python
def add1(x):
  return x + 1
```

It's declared exactly the same way. While there are some differences between monty functions
and python functions for the most part they're very similar.

## Functions as a first class citizens

Your first question might be: "What does 'first class citizens' mean? They look exactly the 
same as in python". Let's talk about how functions are often used in functional languages 
compared to imperative ones using an example.

```python
def foo(a):
  return a(3)
```

This may not be something you're used to seeing in python. The function `foo` takes a single 
argument which appears to be another function. What do you think would happen if we ran `foo(add1)`? 
If you thought it should return `4` then you're right! That's exactly how it works. Let's quickly
look at why that's the case by examining what exactly `add1` (the parameter passed to foo) is.

In the `add1` monty example above we declared the function identically to how would have in python
but monty offers another way to declare functions.

```python
add1 = def (x):
  return x + 1
```

Now this may look a little bit odd but in fact the monty interpreter turns our "normal" declaration
into this form. Here we're creating a function with no name which takes a single number and increments 
it by one (the exact same behaviour as before). We're then assigning that nameless function to a 
variable named `add1`.

So if I were now to ask you what type of value was stored in `add1` I would hope you'd immediatly say:
"Well it's a function with no name. You just told us that". Let's now return to our call `foo(add1)`
you might now be able to better describe what's happening here. We're simply passing a variable into
a function, that variable just happens to be a function. Inside the body of `foo` that nameless function
is assigned to `a` rather than `add1` so we can execute the function as you'd expect by running `a(3)`.

The function `foo` will actually work on **any** function that takes a single number as a parameter.
This type of behaviour is very common in functional programming. You'll encounter lot's of functions
that take other functions as their arguments. These types of functions are called **higher order functions**.

You'll see in the [basic collections](./basic-collections.md) section some of the most common higher
order functions provided by prelude.

## Lambdas

While they might have a fancy name lambdas don't introduce anything we haven't already seen in this
section. They're simply a short form for function declarations. They can be useful for single
use functions that you want to declare inline. Alright, enough chatter! Let's see an example.

```python
# From example 1
def add1(x):
  return x + 1

# From example 2
add1 = def (x):
  return x + 1
  
add1 = (x): x + 1
```

Here we see three ways of declaring the exact same function. The first two we've seen already and
the third introduces the lambda syntax. A lambda is simply a function that only has a single line.
The return value of the function is the result of running that line. We also drop the `def` for brevity.

Assigning a lambda to a variable to create a function isn't where they're very useful, after all
we already know how to declare functions. Let's reuse another example from above however to demonstrate
how they can be useful. However this time we'll use a lambda instead of a regular function call.

```python
def foo(a):
  return a(3)
  
foo((x): x + 1)
```

Notice how we don't declare `add1` as a standalone function anymore? We can simply declare it
directly in our function call. This can be handy in all sorts of different situations often when
you're iterating lists or other containers.
