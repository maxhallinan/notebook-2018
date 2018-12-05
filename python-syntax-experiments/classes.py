class Foo():
    def __init__(self):
        self.foo = 'foo'
    
    def set_bar(self, bar):
        self.bar = bar
        return self

    def set_baz(self, baz):
        self.baz = baz
        return self

foo = Foo()
foo.set_bar('bar').set_baz('baz')
print(foo.bar, foo.baz)
