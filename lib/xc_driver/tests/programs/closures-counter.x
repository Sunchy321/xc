// 闭包按引用捕获：n 的状态跨调用持续（备忘录附录 A 范例一）。
// expect: 1
// expect: 2
func makeCounter() {
    let mut n = 0;
    return do () { n = n + 1; n };
}

let c = makeCounter();
print(c());
print(c());
