// defer 在 return 路径运行：cleanup 先于调用方继续。
// expect: cleanup
// expect: 7
func f() {
    defer {
        print("cleanup");
    }

    return 7;
}

print(f());
