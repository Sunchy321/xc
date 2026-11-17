// 字典变更：插入、覆盖（后写胜）、显示序保持。
// expect: 5
// expect: 9
// expect: {k: 9, n: 1}
let mut d = [:];
d["k"] = 5;
print(d["k"]);
d["k"] = 9;
print(d["k"]);
d["n"] = 1;
print(d);
