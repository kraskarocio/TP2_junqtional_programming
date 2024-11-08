## TP2 Junqtional Programming

## Compile & run

Valid commands to run the code.

```bash
# por ahora se ejecuta así
# sbt "runMain JunqtionalApp opt path_to_json"
sbt "runMain JunqtionalApp .f[1] resources/EX1.json"
```
## paths
json | opt | result


`{ "a" : 1 }` | `.a` | 1

`{ "a" : {"b" : 2} }` | `.a.b` | 2

`{ "a" : {"b" : 2} }` | `.a|.b` | 2

`{ "a" : [10,20] }` | `.a[0]` | 10


