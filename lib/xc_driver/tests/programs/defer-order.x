// defer 逆序执行、函数退出时运行。
// expect: body
// expect: inner
// expect: outer
func f() {
    defer { print("outer"); }
    defer { print("inner"); }
    print("body");
}

f();
