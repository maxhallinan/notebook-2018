# named arguments
def named_args(foo, bar, baz):
    print(foo + bar + baz)

named_args(baz='baz', foo='foo', bar='bar')

# default values
def default_args(foo='Foo', bar='Bar', baz='Baz'):
    print(foo + bar + baz)

default_args()

# arbitrary number of arguments
def args_tuple(*args):
    print(args)

args_tuple()
# -> ()
args_tuple(1,"2",3)
# -> (1,"2",3)
args_tuple(1,"2",3,"4",5)
# -> (1,"2",3,"4",5)

# mixing named and unnamed parameters
def mixed_args(first, *rest):
    print(first, rest)

mixed_args(1)
# -> 1 ()
mixed_args(1, 2, 3)
# -> 1 (2,3)

# arbitrary keyword arguments
# `**rest` is a record
def arbitrary_named_args(first, **rest):
    print(first, rest)

arbitrary_named_args('foo')
# -> 'foo' {}
arbitrary_named_args('foo', bar='bar', baz='baz')
# -> foo {'bar': 'bar', 'baz': 'baz'}
