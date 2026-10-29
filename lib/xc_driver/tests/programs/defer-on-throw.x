// throw 穿越 defer 作用域：cleanup 先行，错误继续向调用方传播。
// expect: cleanup
// expect: 9
func f() {
    defer {
        print("cleanup");
    }

    throw 9;
}

try f() catch e { print(e); }
