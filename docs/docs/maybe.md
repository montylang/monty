---
id: maybe
title: Maybe
---

## Motivation

Let's begin with a quick comparison between how Monty and Python handle `None`.
First let's write a simple validator function in Python. If the number is valid
we return the number otherwise we return `None`. Pretty simple.

```python
def validateNumber(num):
  if num > 2 and num < 10:
    return num
  else:
    return None
```

```python
3 + validateNumber(7) # -> 10
3 + validateNumber(11) # -> TypeError: unsupported operand type(s) for +: 'int' and 'NoneType'
```

We can see that everything is fine when the number is valid but we get an error when the
number is invalid. We'd need to use an if to first check the return value, like so:

```python
result = validateNumber(11)

if result is not None:
  result + 3
else:
  # Handle failure

```

Logic like this can become quite convoluted very quickly. Imagine if we had more numbers
we wanted to operate on. We'd need to check the return value of each `validateNumber` call
before using it. Now let's take a look at rewriting this in monty:

```python
def validateNumber(num):
  if num > 2 and num < 10:
    return Just(num)
  else:
    return None
```

As you can see not much has changed at all, in fact the only thing that's changed is
we've wrapped the `num` return value with `Just`. We'll talk about what exactly that means
but first let's try and use it like we did above.

```python
3 + validateNumber(7) # -> error: cannot operate on types Int and Maybe
```

You'll notice right away there's a difference. Monty seems to be failing even on the "success"
case. Why would that be? Well we're no longer returning `num` we're returning `Just(num)`.

## What is the `Maybe` type?

Let's start off with a definition. In monty (and many other functional languages) we have the
following data type:

```python
class Maybe:
  Just(value)
  None
```

If you're unfamiliar with data types you can learn more about them [here](data-types.md).

The `Maybe` type is used to represent an operation that might fail. Most imperative
languages use `null` to represent this (and python uses `None`). Maybes and nulls are
very similar but maybes offer some advantages that we'll see in a moment.

## Example continued

Let's keep looking at our `validateNumber` function in monty. We saw that we could no
longer operate on the return value the way we would in python. Let's take a look at
how we can unpack a maybe.

```python
case validateNumber(7):
  Just(result): 3 + result
  None: # Handle failure
```

At first glace you might be thinking: "That's even worse than what python does!" and
you'd be right. Luckily we have a few additional tools in functional programming that
allow us to work with a `Maybe` without unwrapping it. The first one we'll look at is
`map`. In [basic collections](basic-collections.md) you should have learnt about `map`
and functors. In monty `Maybe` is a functor i.e. something you can map over. Let's see
how we can use that to our advantage.

```python
result = validateNumber(7).map((x): 3 + x) # Just(10)
```

You'll notice that our result is still wrapped inside of a `Just`. Rather than unpacking
the value we operated on the value inside the `Just`. This has a couple of advantages
that we'll discuss but first let's take a look at what happens when `validateNumber`
fails.

```python
result = validateNumber(11).map((x): 3 + x) # None
```

This is where we start to see the power of using map. Validate number failed and returned
a `None` but calling map on that `None` didn't fail (like you might expect in an imperative
language) instead it "short circuited" and gave us `None` back. In other words we can operate
on the `Maybe` without knowing whether or not it's a `Just` or `None`. We aren't forced to
unwrap the value to operate on it whereas with a null we must check that it's succeeded before
we can use the value.

## A peek at `unwrap`

**Note:** We won't go into how or why `unwrap` works we'll save that for
[a later portion of the docs](monad.md). I would highly suggest not skipping ahead as the
following examples should give you a strong intuition about what's to come.

Let's start with a simple motivational example.

```python
def useValidatedNumber(num):
  return if num == 5:
    Just("Five")
  else:
    None
```

Here we have a function that we want to use on the result of `validateNumber`. Let's
try and use what we just learnt and map that function into the `Maybe`:

```python
result = validateNumber(5).map(useValidatedNumber)
```

Can you see what the problem with this is? It's not immediately obvious so let's
take a look at the returns. Running `validateNumber(5)` gives us `Just(5)` now
we map `useValidatedNumber` inside which runs `useValidatedNumber(5)` which gives
us `Just("Five")` however that result goes back inside the original value giving us a
final value of `Just(Just("Five"))`. This obviously isn't what we want. What we want is
`Just("Five") on it's own`.

Let's take a look at a naive solution:

```python
def newUseValidatedNumber(value):
  return case value:
    Just(num): if num == 5:
                 Just("Five")
               else:
                 None
    None: None
```

We went ahead and rewrote our `useValidatedNumber` to unpack the `Maybe` value
and handle both cases before rewrapping it. This avoids the nested `Just`s but
we're back to where we started. We have to unpack the number before we could use it.

Now let's look at how `unwrap` can help us here:

```python
result = unwrap:
  validated <- validateNumber(5)
  useValidatedNumber(validated)
```

Let's start by looking at what the `<-` does inside of `unwrap` (it can't be used anywhere
else). The `<-` operator can be thought of like a special assignment. The only difference
being that it unwraps the value on the right before assigning it to the variable on the left.
So in our case we know `validateNumber(5)` is `Just(5)` so `validated` will be `5`. You
should immediately be able to see how this solves our previous problem. Now that we have
the inside of our `Just` on it's own we can simply pass that to useValidatedNumber.

The final line of the unwrap is what the entire expression will evaluate to. You can think
of it as a "return" for the unwrap. We know that `useValidatedNumber(5)` is `Just("Five")`
so this unwrap will "return" `Just("Five")`.

At this point you might be wondering what happens if you try and `<-` a `None` value. Let's
take a look:

```python
result = unwrap:
  validated <- validateNumber(11)
  useValidatedNumber(validated)
```

In this case `result` is simply `None`. After `validateNumber(11)` returns `None` we stop
executing and return `None`. This saves us from having to worry about the `None` case in
`useValidatedNumber`.

In addition `unwrap` blocks can have many`<-` lines. This allows you to perform as many
`Maybe` operations in a row as you want. Rather than checking for failure after every single
operation that can fail you can perform all your operations inside an unwrap and check only
once at the very end.

I'll give one more example just to show another way this can be powerful:

```python
result = unwrap:
  v1 <- validateNumber(3)
  v2 <- validateNumber(7)
  Just(v1 + v2)
```

Here we're combining two `Maybe`s into a single one without having to extract the values.
As you might expect if any of the values on the right hand side is `None` then this
entire expression will short circuit and return `None`.

## Next up

The intuition for dealing with nested `Just`s and chaining `Maybe` operations together is
important because `Maybe` is a Monad. The exact same concepts can be applied to any other Monad!

Armed with our knowledge of nesting and chaining we'll examine Monads in more detail and
see some more examples that you're probably already familiar with.

