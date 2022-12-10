# Lambda Calculus Interpreter
This is the first [lab practice](P1_DLP_Q7_2022_23.pdf) for the college course of DLP, which consists of the implementation of a Lambda Calculus interpreter, following the rules [here](summary_of_rules.pdf) explained.

## Technical manual
In this paragraph we will show how to use all the functions implemented for each exercise.

### Multi-line expressions
For this exercise, only the file _main.ml_ was modified, because we decided to directly manipulate the string the user writes. For this, we created a new function called _read\_multi\_lines_, which retrieves every line the user writes on the interpreter, trims and processes it, so if reads a non empty line that doesn't finish with ";", it will concat it to a new instruction string. Then, we call this new function from the _top\_level\_loop_ function. This way, a multi-line expression is allowed.

### Internal fixed point combiner

```
>> sum = letrec sum : Nat -> Nat -> Nat =
lambda n : Nat. lambda m : Nat. if iszero n then m else succ (sum (pred n) m) in
sum
;;
>> sum 21 34;;
- : Nat = 55
```

### Global definitions context
```
>> x = true;;
>> id = lambda x : Bool. x;;
>> id x;;
```

### String type

```
"Cad3n4";
"cad3" ^ "n4";
"" ^ ((Lx:String. x^"by") "ru");;
```

### Pairs type

```
>> p = (2, true);;
>> p2 = (false, p);;
>> pair = (p, p2);;
>> first pair;;
>> second pair;;
```

### Tuples type

```
tuple = {pair, 2, 3};;
tuple.1;;
first (tuple.1);;
```

### Records type

```
record = {a = {a = 1, b = 2}, b = tuple};;
record.a;;
(record.a).b;;
record.b;;
{};;
```
