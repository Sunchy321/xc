// 字典：字面量、下标读、缺失报错路径、长度。
// expect: 1
// expect: 2
// expect: {a: 1, b: 2}
let d = ["a": 1, "b": 2];
print(d["a"]);
print(d["b"]);
print(d);
