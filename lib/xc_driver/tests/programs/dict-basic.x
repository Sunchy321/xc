// 字典：字面量、直接下标读取和长度。
// expect: 2
// expect: 2
// expect: {a: 1, b: 2}
let d = ["a": 1, "b": 2];
print(d["a"] + 1);
print(d["b"]);
print(d);
