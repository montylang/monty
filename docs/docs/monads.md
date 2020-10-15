---
id: monads
title: Simple Monads
---

## Maybe

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
  ...
```

Logic like this can become quite convoluted very quickly. Imagine if we had more numbers
we wanted to operate on. We'd need to check the return value of each `validateNumber` call
before using it. Now let's take a look at monty.

Now let's try rewriting all of this in Monty.

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
case. Why would that be? Well

## Monads you've already seen
