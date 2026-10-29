// 三层捕获链：inner 捕获 outer 的 upval，outer 捕获 f 的参数。
// expect: 13
func adder(n: int) {
    return do (x: int) {
        return do (y: int) {
            x + y + n
        };
    };
}

let g = adder(10);
print(g(1)(2));
