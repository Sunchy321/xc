// 捕获值跨任务链式转移（移动语义：v 在 spawn 后失效，故每段用新绑定）。
// expect: 21
// expect: 42
// expect: 84
let t1 = spawn(do () { return 21; });
let v1 = t1.await;
print(v1);
let t2 = spawn(do () { return v1 * 2; });
let v2 = t2.await;
print(v2);
let t3 = spawn(do () { return v2 * 2; });
print(t3.await);
