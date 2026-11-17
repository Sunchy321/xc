// 任务失败在边界类型化：TaskFailure 进 er，catch 观察（死亡不可恢复）。
// expect: parent
// expect: child cleanup
// expect: boom
let t = spawn(do () {
    defer { print("child cleanup"); }
    panic("boom");
});
print("parent");
try { t.await; print("unreachable"); } catch e { print(e); }
