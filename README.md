# grouped-list

Welcome to the `grouped-list` repository.

We are at an early stage of development, but
contributions are more than welcome. If you
are interested, feel free to submit a pull
request.

# What is this about?

This library defines the type of grouped lists,
``Grouped``. Values of this type are lists
with a finite number of elements. The only
special feature is that consecutive elements
that are equal on the list are internally
represented as a single element annotated
with the number of repetitions. Therefore,
operations on lists that have many consecutive
repetitions perform much better, and memory
usage is reduced. However, this type is suboptimal
for lists that do not have many consecutive
repetitions. We are trying to ameliorate this.
