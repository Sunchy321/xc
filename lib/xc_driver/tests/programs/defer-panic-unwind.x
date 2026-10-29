// panic 展开经 defer：cleanup 运行后进程失败。
// error: panic: boom
func f() {
    defer {
        print("cleanup");
    }

    panic("boom");
}

print("before");
f();
print("unreachable");
