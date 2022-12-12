# Lambda Calculus Interpreter
This is the first [lab practice](P1_DLP_Q7_2022_23.pdf) for the college course of DLP, which consists of the implementation of a Lambda Calculus interpreter, following the rules [here](summary_of_rules.pdf) explained.

## Technical manual
In this paragraph we will show how to use all the functions implemented for each exercise. All the examples shown next (and more), can be found in the _examples.txt_ file.

### Multi-line expressions
For this exercise, only the file _main.ml_ was modified, because we decided to directly manipulate the string the user writes. For this, we created a new function called _read\_multi\_lines_, which retrieves every line the user writes on the interpreter, trims and processes it, so if reads a non empty line that doesn't finish with ";" (no need for ";;", it will stop with just one semicolon), it will concat it to a new instruction string. Then, we call this new function from the _top\_level\_loop_ function. This way, a multi-line expression is allowed.

### Internal fixed point combiner
```
>> sum = letrec sum : Nat -> Nat -> Nat =
lambda n : Nat. lambda m : Nat. if iszero n then m else succ (sum (pred n) m) in
sum
;;
>> sum 21 34;;
```

### Global definitions context
```
>> x = true;;
>> id = lambda x : Bool. x;;
>> id x;;
```

### String type

```
>> "Cad3n4";
>> "cad3" ^ "n4";
>> "" ^ ((Lx:String. x^"by") "ru");;
```

### Pairs type
A pair consists of two elements between curly brackets, and we can access both those elements thanks to having added two new tokens "first" and "second" that go before the pair. We decided to follow this notation and not the one with the tokens ".1" and ".2" after the pair, because we also wanted to implement tuples, so this way we can easily differenciate each function.

```
>> p = {2, true};;
>> p2 = {false, p};;
>> pair = {p, p2};;
>> first pair;;
>> second pair;;
```

### Tuples type and Records type
We write both this sections together, because we followed the same logic to implement them, with the main difference that for tuples we separate each element by commas, and we access each element looking at its index, and for records we separate each element by commas, and each element consists of a tag, followed by the equals token, and followed by the element's value for that tag, and we access each element by looking at its tag.

#### Tuples type

```
>> tuple = {pair, 2, 3};;
>> tuple.1;;
>> first (tuple.1);;
```

#### Records type
As a side note, we can only have a record that it's empty, meaning that if we write two curly brackets together, with no elements, the interpreter will understand that as a record.

```
>> record = {a = {a = 1, b = 2}, b = tuple};;
>> record.a;;
>> (record.a).b;;
>> record.b;;
>> {};;
```

### Exit program
We don't have and expecific command for closing our interpreter, but we can use the default exit shortcut _CTRL + D_. This way, it'll show a goodbye message before closing the program.

## User manual
We use Makefile to build our set up for the program.
To compile:
```
make all
```

To run the interpreter alone:
```
./top
```

To to run it alongside a file:
```
./top < examples.txt
```

To clean up after execution, keeping the executable top:
```
make clean
```

To clean up everything:
```
make cleanall
```
