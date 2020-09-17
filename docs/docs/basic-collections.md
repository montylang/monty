---
id: basic-collections
title: Collections
---

## Types of collections

### The List Collection

In Python, a list is defined as `[1, 2, 3]`. In Monty, it's the exact same syntax. However, every element of the list must be of the same type. So unlike in Python, you cannot have a list such as `[1, 2, "apples"]`.

### The Set Collection

Like with lists, sets must contain all the same type: `Set(1, 2, 3)`

## Working with Collections

### Changing the contents

Suppose you have the following Python code:

```
inputs = [1, 2, 3]
results = [x + 1 for x in results]
```

The `map` function takes in the collection, then a function to change the contents. This is how you change values of a collection in functional languages.

So let's rewrite it:

```
inputs = [1, 2, 3]
results = inputs.map((x): x + 1)
```

Remember, we can write functions inline using the syntax `(x): x + 1`

### Accumulating results

Suppose you have the following Python code:

```
inputs = [1, 2, 3]
result = 0

for x in inputs:
  result += x
```

Fairly simple, it adds all the results from `inputs` up into `result`. In Monty, this code becomes a problem. Remember, we can't change `result` after we define it.

Let's reimplement the above Python code to not change `result`:

```
def addAll(inputs):
  if inputs == []:
    return 0

  first = inputs[0]
  rest  = inputs[1:]
  
  return first + addAll(rest)
  
result = addAll(inputs)
```

OK that should be fine in Monty, since nothing changes after being defined.

"But it's clunky! I like the first example waaaaaaaaaay more!"

I agree with you. Let's create a function called `fold` that will help us with this. I want something generalized to take in a function, which combines all of the values together. It will also need an accumulator parameter

```
def fold(inputs, accumulator, func):
  if inputs == []:
    return accumulator

  first = inputs[0]
  rest  = inputs[1:]
  
  newAccumulator = func(accumulator, first)

  return fold(rest, newAccumulator, func)
```

Now with this function, we can clean up the immutable Python code with:

```
def add(x, y):
  return x + y

result = fold(inputs, 0, add)
```

Much nicer! And we can easily change what that function does.

In Monty, lists have a `foldl` method out of the box, so rewriting this in Monty:

```
inputs = [1, 2, 3]
result = inputs.foldl(0, (x, y): x + y)
```

### The Maybe Collection

## Functors

"Whoa whoa whoa, what's a Functor?! Why'd you have to drop that bomb on me!"

Stay calm! A Functor is just something that you can `map` over.

"Whew ok, that's not so bad"

For the examples given... for loops seem simpler than using a map, don't you agree?

### Trees

Say you have this code in Python:

```
class BTree:
  def __init__(self, value, left, right):
    self.value = value
    self.left  = left
    self.right = right
    
inputs = BTree(1, BTree(2, None, None), BTree(3, None, None))
```

How would you increment all the values, as we did with the list example?

"Let's see... looks like we'll need a recursive function, something like"

```
def add1(t):
  if t == None:
    return None
    
  return BTree(t.value + 1, add1(t.left), add1(t.right))

results = add1(inputs)
```

"That wasn't so bad."

Sorry, I actually have a tree that has any number of branches. Can you rewrite `add1` for that please?

"OK, I'll just need to change add1 a bit..."

Before going on with this charade, let's talk about doing this in Monty instead of Python. This is a Monty book after all.

Let's rewrite `BTree`:

```
class BTree:
  Node(value, left, right)
  Leaf
```

OK, `add1` can be implemented as:

```
def add1(t):
  if t == Leaf:
    return Leaf

  return Node(t.value + 1, add1(t.left), add1(t.right))
```

It's almost identical to the Python code, and it has the same problem as the Python code. Now, remember that a Functor is something you can `map` over. What if we could `map` over this tree? Then we could use the exact same code to increment all the values as with the list:

```
inputs = Node(1, Node(2, Leaf, Leaf), BTree(3, Leaf, Leaf))
results = inputs.map((x): x + 1)
```

OK now make inputs a list again.

"OK that's fine, none of the code for `results` will change`

But wait, how does `BTree` magically become a Functor? Sit tight until the chapter on `instances` and you'll find out!

### Foldables

A Foldable is something you can call `foldl` on, such as lists.

Now, going back to the accumulating list example. What if we wanted to accumulate the contents of a tree in Python? Like for the `add1` example for Functors above, it would require some fancy logic.

Remember what `foldl` does. It goes over a collection, and gathers all the contents into a single value.

Guess what? A Tree is foldable! So we use the exact same code as for lists:

```
inputs = Node(1, Node(2, Leaf, Leaf), BTree(3, Leaf, Leaf))
result = inputs.foldl(0, (x, y): x + y)
```
