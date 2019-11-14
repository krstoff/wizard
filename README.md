wizard is a notation for magic.
--------------------------------

## Values
- strings
"abcdefg"

- numbers
1 2 3 -4
1.0 0.4 -10.0

- symbols
:name ::whatever/you/want


- lists
[1, 2, 3, 4]

- property lists
[:author "Bob Avakian", :title "The New Synthesis"]
{ author: "Bob", title: "The New Synthesis"}

- structs
type book(:author, :title)
book(:author "Michael Parenti", :title "Blackshirts and Reds")

- functions
fun(x) { x + 2 }

## Expressions
- identifier
local-name

- literal (see above)

- math
a + b - x * 4 / 1 % size

- logic (chained comparisons a la python)
2 < 4
4 <= 4
0 < i <= 12
on && ready

- indexing
fruits[12]

- property access
robot::test
You need virtual properties someday.

-  function calls
do-thing("string")
string/concat("string", "string")
list(:with-capacity 50)

- pipe operator
"string" .do-thing()
h .hashmap/keys()
list .find-all(even)s

- parens
(a + b) * c

+ hint, cast
num of int
integer as float

+ blocks
{
    let x = 1
    x + 2     # last expression is returned
}

- assignment
x = 12
book::isbn = 15
list[40] = 10

## control flow
All of the following are still expressions.

return x

if 2 < x < 10 { 10 }
else { 5 }

if x.bad() { return bad-error(x) }

while iter.running() {
    iter .next()
}
loop get-it()
loop { do-it() }

for x in [0, 5, 10] { print(x) }
while iter.running() {
  if signal.recieve() {
    break(help)
  }
}
break 19
continue

try {
    let data = download_files()
    let findings = analyze(data)
    print(findings)
}

## patterns
count ? {
  0 -> { panic(:empty) }
  n -> { prepare_tasks() }
}

report ? {
  if report::processing -> { whatever }
  r if r::special -> {
    log-report(r)
    handle-special-report(r)
  }
  r -> { graph(r::data) }
}

list ? {
  [] ->
  [a, b, ...rest] ->
  _ -> do something else
}

db-find(thing) ? {
  e of auth-err -> { permissions-denied() }
  e of error -> { log(e); return }
  val -> do-it()
}

alternatively, you can write
let result = db-find(thing)
result ? {
  of auth-err -> { permissions-denied() }
  of error -> { log(result); return }
  _ -> do-it()
}
